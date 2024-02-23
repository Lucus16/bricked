{-# LANGUAGE OverloadedStrings #-}

module Bricked.Expr where

import Control.Arrow (first, (>>>))
import Control.Monad.State.Strict (evalState, runState, state)
import Data.Fix (Fix (..), foldFix, hoistFix)
import Data.Functor ((<&>))
import Data.Functor.Reverse (Reverse (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty.Attributes
import Graphics.Vty.Image

type Name = Text

data ExprF e
  = Where e [(Name, e)]
  | Var Name
  | Int Integer
  | Update e [e]
  | BinUp e e
  | Add
  | Sub
  | Mul
  | Div
  deriving (Functor, Foldable, Traversable)

type Expr = Fix ExprF

space :: Image
space = text defAttr " "

renderExpr :: Int -> Expr -> Image
renderExpr = foldFix . renderExprF

renderExprF :: Int -> ExprF Image -> Image
renderExprF maxWidth = \case
  Where e binds ->
    e
      <-> text defAttr "where"
      <-> vertCat (renderBinding <$> binds)
  Var name -> text' defAttr name
  Int i -> text' defAttr $ tshow i
  Update e fs ->
    (e <|> horizCat ((space <|>) <$> fs))
      `orVertical` (e <-> vertCat fs)
  BinUp op e -> op <|> space <|> e
  Add -> text defAttr "+"
  Sub -> text defAttr "-"
  Mul -> text defAttr "*"
  Div -> text defAttr "/"
  where
    renderBinding (name, value) =
      (text' defAttr name <|> text defAttr " = " <|> value)
        `orVertical` (text' defAttr name <-> (text defAttr "= " <|> value))

    orVertical hImage vImage
      | imageWidth hImage <= maxWidth && imageHeight hImage <= 1 = hImage
      | otherwise = vImage

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

data SelF f r
  = Selected (Fix f)
  | NotSelected (Fix f)
  | PartiallySelected (f r)
  deriving (Functor)

type Sel f = Fix (SelF f)

type SelExpr = Sel ExprF

unSel :: (Functor f) => Sel f -> Fix f
unSel = foldFix \case
  Selected x -> x
  NotSelected x -> x
  PartiallySelected fx -> Fix fx

hoistSel :: (Functor f) => (forall a. f a -> g a) -> Sel f -> Sel g
hoistSel nt = hoistFix \case
  Selected x -> Selected $ hoistFix nt x
  NotSelected x -> NotSelected $ hoistFix nt x
  PartiallySelected x -> PartiallySelected $ nt x

reverseSel :: (Functor f) => (Sel (Reverse f) -> Sel (Reverse f)) -> Sel f -> Sel f
reverseSel f = hoistSel getReverse . f . hoistSel Reverse

mapSel :: (Functor f) => (f (Sel f) -> f (Sel f)) -> Sel f -> Sel f
mapSel f = Fix . go . unFix
  where
    go = \case
      Selected x -> Selected x
      NotSelected x -> NotSelected x
      PartiallySelected tx -> PartiallySelected $ mapSel f <$> f tx

select :: (Functor f) => Sel f -> Sel f
select = Fix . Selected . unSel

deselect :: (Functor f) => Sel f -> Sel f
deselect = Fix . NotSelected . unSel

data Select = Select | Deselect

selectNextOrNone :: (Traversable t) => Sel t -> Sel t
selectNextOrNone = mapSel $ fst . genericSelectNext

selectNextOrSame :: (Traversable t) => Sel t -> Sel t
selectNextOrSame = mapSel \x -> case genericSelectNext x of
  (x', Select) -> alsoSelectLast x'
  (x', Deselect) -> x'

selectPrevOrNone :: (Traversable t) => Sel t -> Sel t
selectPrevOrNone = reverseSel selectNextOrNone

selectPrevOrSame :: (Traversable t) => Sel t -> Sel t
selectPrevOrSame = reverseSel selectNextOrSame

genericSelectNext
  :: (Traversable t, Functor f)
  => t (Sel f)
  -> (t (Sel f), Select)
genericSelectNext =
  flip runState Deselect . traverse \x -> state \selThis ->
    first Fix case (unFix x, selThis) of
      (Selected fx, Select) -> (Selected fx, Select)
      (Selected fx, Deselect) -> (NotSelected fx, Select)
      (NotSelected fx, Select) -> (Selected fx, Deselect)
      (NotSelected fx, Deselect) -> (NotSelected fx, Deselect)
      (PartiallySelected fx, Select) -> (Selected (unSel x), Deselect)
      (PartiallySelected fx, Deselect) -> (PartiallySelected fx, Deselect)

alsoSelectFirst :: (Traversable t, Functor f) => t (Sel f) -> t (Sel f)
alsoSelectFirst =
  flip evalState Select . traverse \x -> state \case
    Select -> (select x, Deselect)
    Deselect -> (x, Deselect)

alsoSelectLast :: (Traversable t, Functor f) => t (Sel f) -> t (Sel f)
alsoSelectLast = getReverse . alsoSelectFirst . Reverse
