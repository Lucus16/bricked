{-# LANGUAGE OverloadedStrings #-}

module Bricked.Expr where

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

data Sel f
  = Selected (Fix f)
  | Unselected (Fix f)
  | PartiallySelected (f (Sel f))

type SelExpr = Sel ExprF

unSel :: (Functor f) => Sel f -> Fix f
unSel = \case
  Selected x -> x
  Unselected x -> x
  PartiallySelected fx -> Fix $ unSel <$> fx

hoistSel :: (Functor f) => (forall a. f a -> g a) -> Sel f -> Sel g
hoistSel nt = \case
  Selected x -> Selected $ hoistFix nt x
  Unselected x -> Unselected $ hoistFix nt x
  PartiallySelected x -> PartiallySelected $ nt $ hoistSel nt <$> x

reverseSel
  :: (Functor t, Functor t')
  => (Reverse t (Sel (Reverse t')) -> Reverse t (Sel (Reverse t')))
  -> (t (Sel t') -> t (Sel t'))
reverseSel f = fmap (hoistSel getReverse) . getReverse . f . Reverse . fmap (hoistSel Reverse)

-- TODO: Make the functions below non-recursive and make a recursion combinator

data Select = Select | Deselect

genericReselect
  :: (Traversable t, Traversable t')
  => Select
  -> t (Sel t')
  -> (t (Sel t'), Select)
genericReselect selFirst =
  flip runState selFirst . traverse \x -> state \selThis ->
    case (x, selThis) of
      (Selected fx, Select) -> (Selected fx, Select)
      (Selected fx, Deselect) -> (Unselected fx, Select)
      (Unselected fx, Select) -> (Selected fx, Deselect)
      (Unselected fx, Deselect) -> (Unselected fx, Deselect)
      (PartiallySelected fx, Select) -> (Selected (unSel x), Deselect)
      (PartiallySelected fx, Deselect) -> (PartiallySelected fx, Deselect)

selectNextOrNone :: (Traversable t, Traversable t') => t (Sel t') -> t (Sel t')
selectNextOrNone = fst . genericReselect Deselect

selectNextOrSame :: (Traversable t, Traversable t') => t (Sel t') -> t (Sel t')
selectNextOrSame x = case genericReselect Deselect x of
  (x', Select) -> alsoSelectLast x'
  (x', Deselect) -> x'

alsoSelectFirst :: (Traversable t, Functor f) => t (Sel f) -> t (Sel f)
alsoSelectFirst =
  flip evalState Select . traverse \x -> state \selThis ->
    case (x, selThis) of
      (Selected fx, Select) -> (Selected fx, Deselect)
      (Unselected fx, Select) -> (Selected fx, Deselect)
      (PartiallySelected fx, Select) -> (Selected (unSel x), Deselect)
      (Selected fx, Deselect) -> (Selected fx, Deselect)
      (Unselected fx, Deselect) -> (Unselected fx, Deselect)
      (PartiallySelected fx, Deselect) -> (PartiallySelected fx, Deselect)

selectPrevOrNone :: (Traversable t, Traversable t') => t (Sel t') -> t (Sel t')
selectPrevOrNone = reverseSel selectNextOrNone

selectPrevOrSame :: (Traversable t, Traversable t') => t (Sel t') -> t (Sel t')
selectPrevOrSame = reverseSel selectNextOrSame

alsoSelectLast :: (Traversable t, Functor f) => t (Sel f) -> t (Sel f)
alsoSelectLast = reverseSel alsoSelectFirst
