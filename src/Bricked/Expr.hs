{-# LANGUAGE OverloadedStrings #-}

module Bricked.Expr
  ( JsonF (..)
  , Json
  , SelF (..)
  , Sel
  , SelJson
  , jsonSelectKeys
  , Fix (..)
  , Render (..)
  , select
  , deselect
  , selectAllParents
  , selectAllChildren
  , selectNextOrNone
  , selectNextOrSame
  , selectPrevOrNone
  , selectPrevOrSame
  ) where

import Control.Arrow (first, (>>>))
import Control.Monad.State.Strict (evalState, runState, state)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Fix (Fix (..), foldFix, hoistFix)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Functor.Classes (Show1)
import Data.Functor.Reverse (Reverse (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Graphics.Vty.Attributes
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Image
import Graphics.Vty.Image.Internal qualified as Vty

data JsonF e
  = Null
  | Bool Bool
  | Number Text
  | String Text
  | Array [e]
  | KeyValue e e
  | Object [e]
  | Comment Text
  deriving (Functor, Foldable, Traversable)

instance Aeson.FromJSON (JsonF (Fix JsonF)) where
  parseJSON = \case
    Aeson.Null -> pure Null
    Aeson.Bool b -> pure $ Bool b
    Aeson.Number n -> pure $ Number $ tshow n
    Aeson.String t -> pure $ String t
    Aeson.Array xs -> Array . toList <$> traverse (fmap Fix . Aeson.parseJSON) xs
    Aeson.Object o -> fmap Object do
      for (KeyMap.toList o) \(key, value) ->
        Fix . KeyValue (Fix $ String $ Key.toText key) . Fix <$> Aeson.parseJSON value

type Json = Fix JsonF

class Render1 f where
  render1 :: (Int -> a -> Image) -> Int -> f a -> Image
  debug1 :: (a -> Image) -> f a -> Image

class Render a where
  render :: Int -> a -> Image
  debug :: a -> Image

instance (Render1 f) => Render (Fix f) where
  render maxWidth (Fix x) = render1 render maxWidth x
  debug (Fix x) = debug1 debug x

instance (Render1 f, Render a) => Render (f a) where
  render = render1 render
  debug = debug1 debug

instance Render1 JsonF where
  render1 renderX maxWidth = \case
    Null -> literal "null"
    Bool False -> literal "false"
    Bool True -> literal "true"
    Number t -> literal t
    String t -> case Text.lines t of
      [] -> syntax ""
      [line] -> literal line
      lines -> vertCat $ (syntax "│ " <|>) . literal <$> lines
    Comment t -> comment t
    Array [] -> syntax "[]"
    Array xs ->
      let x_ : xs_ = renderX (maxWidth - 2) <$> xs
       in (syntax "[" <|> x_ <|> horizCat ((syntax ", " <|>) <$> xs_) <|> syntax "]")
            `orVertical` vertCat ((syntax "• " <|>) <$> x_ : xs_)
    KeyValue name value ->
      let name_ = renderX (maxWidth - 1) name
          value_ = renderX (maxWidth - 2) value
       in (name_ <|> syntax ": " <|> value_)
            `orVertical` (name_ <|> syntax ":" <-> backgroundFill 2 1 <|> value_)
    Object xs -> vertCat $ renderX maxWidth <$> xs
    where
      orVertical hImage vImage
        | imageWidth hImage <= maxWidth && imageHeight hImage <= 1 = hImage
        | otherwise = vImage

      literal = text' $ currentAttr `withForeColor` brightMagenta
      syntax = text' $ currentAttr `withForeColor` yellow
      comment = text' $ currentAttr `withForeColor` brightWhite

  debug1 debugX = \case
    Null -> text defAttr "Null"
    Bool _ -> text defAttr "Bool"
    Number _ -> text' defAttr "Number"
    String _ -> text defAttr "String"
    Comment _ -> text defAttr "Comment"
    Array xs -> debugNode "Array" $ vertCat $ debugX <$> xs
    KeyValue k v -> debugNode "KeyValue" $ debugX k <-> debugX v
    Object xs -> debugNode "Object" $ vertCat $ debugX <$> xs

instance (Render1 f) => Render1 (SelF f) where
  render1 renderX maxWidth = \case
    Selected x -> attr (currentAttr `withBackColor` ISOColor 238) $ render maxWidth x
    NotSelected x -> render maxWidth x
    PartiallySelected x -> render1 renderX maxWidth x

  debug1 debugX = \case
    Selected x -> debugNode "Selected" $ debug x
    NotSelected x -> debugNode "NotSelected" $ debug x
    PartiallySelected fx -> debugNode "PartiallySelected" $ debug1 debugX fx

debugNode :: Text -> Image -> Image
debugNode name contents
  | isEmptyImage contents = text' defAttr name
  | otherwise = text' defAttr name <-> backgroundFill 2 1 <|> contents

isEmptyImage :: Image -> Bool
isEmptyImage img = imageWidth img == 0 && imageHeight img == 0

attr :: Attr -> Image -> Image
attr a img = vertCat (replicate (imageHeight img) attrStart) <|> img <|> attrEnd
  where
    attrStart =
      Vty.HorizText
        { Vty.attr = a
        , Vty.displayText = ""
        , Vty.outputWidth = 0
        , Vty.charWidth = 0
        }

    attrEnd =
      Vty.HorizText
        { Vty.attr = defAttr
        , Vty.displayText = ""
        , Vty.outputWidth = 0
        , Vty.charWidth = 0
        }

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

jsonSelectKeys :: SelJson -> SelJson
jsonSelectKeys = mapSelected jsonSelectKeys'

jsonSelectKeys' :: Json -> Sel JsonF
jsonSelectKeys' (Fix json) = Fix case json of
  KeyValue k v -> PartiallySelected $ KeyValue (Fix $ Selected k) (jsonSelectKeys' v)
  x -> PartiallySelected $ jsonSelectKeys' <$> x

-- TODO: I might need to allow nested selections. I would need up to 3 shades of
-- gray to distinguish every selection.

-- TODO: To properly support nested vty Images, I'll need to change the vty
-- library to move Attr out of HorizText and into a wrapper constructor.

data SelF f r
  = Selected (Fix f)
  | NotSelected (Fix f)
  | PartiallySelected (f r)
  deriving (Functor)

type Sel f = Fix (SelF f)

type SelJson = Sel JsonF

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

mapSelected :: (Functor f) => (Fix f -> Sel f) -> Sel f -> Sel f
mapSelected f = foldFix \case
  Selected x -> f x
  NotSelected x -> Fix $ NotSelected x
  PartiallySelected fx -> Fix $ PartiallySelected fx

select :: (Functor f) => Sel f -> Sel f
select = Fix . Selected . unSel

deselect :: (Functor f) => Sel f -> Sel f
deselect = Fix . NotSelected . unSel

data Select = Select | Deselect

selectAllChildren :: (Functor f) => Sel f -> Sel f
selectAllChildren = mapSelected \(Fix x) -> Fix $ PartiallySelected $ Fix . Selected <$> x

selectAllParents :: (Functor f, Foldable f) => Sel f -> Sel f
selectAllParents = Fix . go . unFix
  where
    go = \case
      Selected x -> Selected x
      NotSelected x -> NotSelected x
      PartiallySelected fx ->
        if any (isSelected . unFix) fx
          then Selected $ Fix $ unSel <$> fx
          else PartiallySelected $ selectAllParents <$> fx

    isSelected Selected{} = True
    isSelected _ = False

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
