{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad ((>=>), void)
import Cursor.List
import Cursor.Text
import Cursor.TextField
import Cursor.Types qualified as Cursor
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Zipper (TextZipper, stringZipper, textZipper)
import Data.Text.Zipper qualified as TextZipper
import Data.Time.Clock (UTCTime)
import Data.Tuple (swap)
import Data.Word (Word64)
import System.Directory ()
import System.Environment (getArgs)
import System.Exit ()
import Text.Read (readMaybe)
import Data.List.Zipper qualified as Zipper
import Data.List.Zipper (Zipper)

import Brick (App(..), Location(..), continue, halt, vBox)
import Brick qualified as Brick
import Brick.Main qualified as Brick
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Brick.Widgets.Core qualified as Brick

import Graphics.Vty.Attributes qualified as Vty
import Graphics.Vty.Input.Events qualified as Vty

type Widget = Brick.Widget Text

newtype Password = Password ByteString

data Session = Session
  { sessionSecret    :: Text
  , sessionExpiresAt :: UTCTime
  }

data Account = Account
  { accName     :: Text
  , accId       :: Word64
  , accRole     :: Role
  , accPassword :: Password
  , accSessions :: [Session]
  }

data Coordinate a = Coordinate a a

class Editable a where
  data Exposed a
  blank :: Exposed a
  expose :: a -> Exposed a
  suggest :: Exposed a -> [Exposed a]
  suggest = const []
  assemble :: Exposed a -> Maybe a
  handleKey :: [Vty.Modifier] -> Vty.Key -> Exposed a -> Maybe (Exposed a)
  drawExposed :: Exposed a -> Widget
  drawAssembled :: a -> Widget

drawLine :: Zipper Char -> Widget
drawLine = Brick.showCursor "cursor" . Location . (,0) . length . Zipper.before
  <*> Brick.strWrap . Zipper.toList

instance Editable Text where
  newtype Exposed Text = ExposedText { unExposedText :: TextZipper Text }
  blank = expose ("" :: Text)
  expose t = ExposedText $ textZipper (Text.lines t) Nothing
  suggest = const []
  assemble = Just . Text.unlines . TextZipper.getText . unExposedText
  handleKey mods key = fmap ExposedText . handleKeyForTextZipper mods key . unExposedText
  drawExposed = Brick.showCursor "cursor" . Location . swap . TextZipper.cursorPosition . unExposedText
    <*> Brick.txtWrap . Text.unlines . TextZipper.getText . unExposedText
  drawAssembled = Brick.txtWrap

handleKeyForTextZipper :: (Eq a, Monoid a) => [Vty.Modifier] -> Vty.Key -> TextZipper a -> Maybe (TextZipper a)
handleKeyForTextZipper []          (Vty.KChar c)   = Just . TextZipper.insertChar c
handleKeyForTextZipper [Vty.MCtrl] (Vty.KChar 'a') = Just . TextZipper.gotoBOL
handleKeyForTextZipper [Vty.MCtrl] (Vty.KChar 'e') = Just . TextZipper.gotoEOL

handleKeyForTextZipper [] Vty.KBS    = Just . TextZipper.deletePrevChar
handleKeyForTextZipper [] Vty.KDel   = Just . TextZipper.deleteChar
handleKeyForTextZipper [] Vty.KDown  = Just . TextZipper.moveDown
handleKeyForTextZipper [] Vty.KEnd   = Just . TextZipper.gotoEOL
handleKeyForTextZipper [] Vty.KEnter = Just . TextZipper.breakLine
handleKeyForTextZipper [] Vty.KHome  = Just . TextZipper.gotoBOL
handleKeyForTextZipper [] Vty.KLeft  = Just . TextZipper.moveLeft
handleKeyForTextZipper [] Vty.KRight = Just . TextZipper.moveRight
handleKeyForTextZipper [] Vty.KUp    = Just . TextZipper.moveUp
handleKeyForTextZipper _ _           = const Nothing

handleKeyForLine :: [Vty.Modifier] -> Vty.Key -> Zipper Char -> Maybe (Zipper Char)
handleKeyForLine []          (Vty.KChar c)   = Just . Zipper.insert c
handleKeyForLine [Vty.MCtrl] (Vty.KChar 'a') = Zipper.toStart
handleKeyForLine [Vty.MCtrl] (Vty.KChar 'e') = Zipper.toEnd

handleKeyForLine [] Vty.KLeft  = Zipper.toPrev
handleKeyForLine [] Vty.KRight = Zipper.toNext
handleKeyForLine [] Vty.KHome  = Zipper.toStart
handleKeyForLine [] Vty.KEnd   = Zipper.toEnd
handleKeyForLine [] Vty.KBS    = Zipper.toPrev >=> Zipper.delete
handleKeyForLine [] Vty.KDel   = Zipper.delete
handleKeyForLine _ _           = const Nothing

dropCursor :: Widget -> Widget
dropCursor p = Brick.Widget (Brick.hSize p) (Brick.vSize p) $ updateResult <$> (Brick.render p)
  where
    updateResult result = result { Brick.cursors = drop 1 (Brick.cursors result) }

exposeShow :: Show a => a -> Zipper Char
exposeShow = Zipper.fromList . show

assembleRead :: Read a => Zipper Char -> Maybe a
assembleRead = readMaybe . Zipper.toList

newtype ReadShowEditable a = ReadShowEditable a
  deriving newtype (Read, Show)

instance (Read a, Show a) => Editable (ReadShowEditable a) where
  newtype Exposed (ReadShowEditable a) = ExposedByReadShow { unExposedByReadShow :: Zipper Char }
  blank = ExposedByReadShow Zipper.empty
  expose = ExposedByReadShow . Zipper.fromList . show
  suggest = const []
  assemble = readMaybe . Zipper.toList . unExposedByReadShow
  handleKey mods key = fmap ExposedByReadShow . handleKeyForLine mods key . unExposedByReadShow
  drawExposed = drawLine . unExposedByReadShow
  drawAssembled = Brick.strWrap . show

--deriving via (ReadShowEditable Int) instance Editable Int

instance Editable Int where
  newtype Exposed Int = ExposedInt { unExposedInt :: Zipper Char }
  blank = expose (0 :: Int)
  expose = ExposedInt . exposeShow
  assemble = assembleRead . unExposedInt
  handleKey mods key = fmap ExposedInt . handleKeyForLine mods key . unExposedInt
  drawExposed = drawLine . unExposedInt
  drawAssembled = Brick.strWrap . show

instance Editable Word64 where
  newtype Exposed Word64 = ExposedWord64 { unExposedWord64 :: Zipper Char }
  blank = expose (0 :: Word64)
  expose = ExposedWord64 . exposeShow
  assemble = assembleRead . unExposedWord64
  handleKey mods key = fmap ExposedWord64 . handleKeyForLine mods key . unExposedWord64
  drawExposed = drawLine . unExposedWord64
  drawAssembled = Brick.strWrap . show

data Role
  = User
  | Admin
  deriving (Bounded, Enum, Read, Show)

instance Editable Role where
  newtype Exposed Role = ExposedRole { unExposedRole :: Zipper Char }
  blank = ExposedRole Zipper.empty
  expose = ExposedRole . exposeShow
  suggest _ = map expose [minBound..maxBound]
  assemble = assembleRead . unExposedRole
  handleKey mods key = fmap ExposedRole . handleKeyForLine mods key . unExposedRole
  drawExposed = drawLine . unExposedRole
  drawAssembled = Brick.strWrap . show

instance Editable a => Editable [a] where
  newtype Exposed [a] = ExposedList { unExposedList :: Zipper (Node a) }
  blank = ExposedList Zipper.empty
  expose = ExposedList . Zipper.fromList . map Complete
  assemble = traverse assembleNode . Zipper.toList . unExposedList

  handleKey mods key = fmap ExposedList . handle . unExposedList
    where
      handle xs = case Zipper.pop xs of
        Just (Exposed x, xs') -> case handleKey mods key x of
          Just x' -> Just $ Zipper.insert (Exposed x') xs'
          Nothing -> handleAsList mods key xs
        Just (Complete x, xs') -> handleAsList mods key xs
        Nothing -> handleAsList mods key xs

      handleAsList [] Vty.KUp = Zipper.toPrev
      handleAsList [] Vty.KDown = Zipper.toNext
      handleAsList _ _ = const Nothing

  drawExposed (ExposedList xs) = vBox $ case Zipper.current xs of
    Nothing  -> before ++ after
    Just cur -> before ++ drawNode cur : after
    where
      before = dropCursor . drawNode <$> Zipper.before xs
      after = dropCursor . drawNode <$> Zipper.after xs
      drawNode = drawExposed . unpack
  drawAssembled = vBox . map drawAssembled

instance Editable UTCTime where
  newtype Exposed UTCTime = ExposedUTCTime { unExposedUTCTime :: Zipper Char }
  blank = ExposedUTCTime Zipper.empty
  expose = ExposedUTCTime . exposeShow
  assemble = assembleRead . unExposedUTCTime
  handleKey mods key = fmap ExposedUTCTime . handleKeyForLine mods key . unExposedUTCTime
  drawExposed = drawLine . unExposedUTCTime
  drawAssembled = Brick.strWrap . show

data Node a
  = Exposed !(Exposed a)
  | Complete !a

pack :: Editable a => Exposed a -> Node a
pack x = maybe (Exposed x) Complete $ assemble x

deselectNode :: Editable a => Node a -> Node a
deselectNode x@(Complete _) = x
deselectNode x@(Exposed ex) = maybe x Complete $ assemble ex

selectNode :: Editable a => Node a -> Node a
selectNode x@(Exposed _) = x
selectNode (Complete x) = Exposed $ expose x

unpack :: Editable a => Node a -> Exposed a
unpack (Exposed x) = x
unpack (Complete x) = expose x

assembleNode :: Editable a => Node a -> Maybe a
assembleNode (Exposed x) = assemble x
assembleNode (Complete x) = Just x

drawNode :: Editable a => Node a -> Widget
drawNode (Exposed x) = drawExposed x
drawNode (Complete x) = drawAssembled x

-- Resolves to a
data EditingBefore a
  = Constructor a
  | forall i. Editable i => BeforeField (EditingBefore (i -> a)) (Node i)

-- Resolves to x -> a
data EditingAfter a where
  Finisher   :: (x -> a) -> EditingAfter (x -> a)
  AfterField :: Editable i => Node i -> EditingAfter (x -> a) -> EditingAfter ((i -> x) -> a)

data EditingRecord o = forall a i. Editable i => EditingRecord
  { erBefore  :: EditingBefore (i -> a)
  , erCurrent :: Exposed i
  , erAfter   :: EditingAfter (a -> o)
  }

recordNextField :: EditingRecord a -> Maybe (EditingRecord a)
recordNextField (EditingRecord before current (Finisher id)) = Nothing
recordNextField (EditingRecord before current (AfterField newCurrent after)) =
  Just $ EditingRecord
    { erBefore = BeforeField before (pack current)
    , erCurrent = unpack newCurrent
    , erAfter = after
    }

recordPrevField :: EditingRecord a -> Maybe (EditingRecord a)
recordPrevField (EditingRecord (Constructor c) current after) = Nothing
recordPrevField (EditingRecord (BeforeField before newCurrent) current after) = do
  Just $ EditingRecord
    { erBefore = before
    , erCurrent = unpack newCurrent
    , erAfter = AfterField (pack current) after
    }

assembleBefore :: EditingBefore a -> Maybe a
assembleBefore (Constructor c) = pure c
assembleBefore (BeforeField before f) = assembleBefore before <*> assembleNode f

assembleAfter :: EditingAfter (a -> b) -> Maybe (a -> b)
assembleAfter (Finisher f) = pure f
assembleAfter (AfterField f after) = do
  af <- assembleNode f
  aafter <- assembleAfter after
  pure \c -> aafter (c af)

assembleEditingRecord :: EditingRecord a -> Maybe a
assembleEditingRecord EditingRecord { erBefore, erCurrent, erAfter } = do
  c <- assembleBefore erBefore
  x <- assemble erCurrent
  f <- assembleAfter erAfter
  pure $ f $ c x

instance Editable Session where
  newtype Exposed Session = ExposedSession { unExposedSession :: EditingRecord Session }
  blank = ExposedSession EditingRecord
    { erBefore = Constructor Session
    , erCurrent = blank
    , erAfter = AfterField (pack blank) (Finisher id)
    }

  expose session = ExposedSession EditingRecord
    { erBefore = Constructor Session :: EditingBefore (Text -> UTCTime -> Session)
    , erCurrent = expose $ sessionSecret session :: Exposed Text
    , erAfter = AfterField (Complete (sessionExpiresAt session)) (Finisher id)
    }

  assemble = assembleEditingRecord . unExposedSession

instance Editable a => Editable (Coordinate a) where
  newtype Exposed (Coordinate a) = ExposedCoordinate { unExposedCoordinate :: EditingRecord (Coordinate a) }
  blank = ExposedCoordinate EditingRecord
    { erBefore = Constructor Coordinate
    , erCurrent = blank
    , erAfter = AfterField (pack blank) (Finisher id)
    }

  expose (Coordinate x y) = ExposedCoordinate EditingRecord
    { erBefore = Constructor Coordinate
    , erCurrent = expose x
    , erAfter = AfterField (Complete y) (Finisher id)
    }

  assemble = assembleEditingRecord . unExposedCoordinate

--instance Editable Account where
--  edit = record Account
--    <$> field "name"
--    <*> field "id"
--    <*> field "role"
--    <*> field "password"
--
--instance Editable Word64 where
--  edit = editReadMaybe
--
--instance Editable Text where

-- As soon as you navigate into something, it is split into its components.

data EditingShowRead a = (Show a, Read a) => EditingShowRead Text

--data Exposed a
--  = Missing
--  | EditingText Text
--  | EditingList [b] (Exposed b) [b] ([b] -> a)
--  | EditingRecord
--  | Complete a

app :: (Editable a) => Brick.App (Exposed a) e Text
app = Brick.App
  { appDraw = pure . drawExposed
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const $ Brick.attrMap Vty.defAttr []
  }

handleEvent :: (Editable a) => Exposed a -> Brick.BrickEvent Text e -> Brick.EventM Text (Brick.Next (Exposed a))
handleEvent tz (Brick.VtyEvent (Vty.EvKey Vty.KEsc mods)) = halt tz
handleEvent tz (Brick.VtyEvent (Vty.EvKey key mods)) =
  continue $ fromMaybe tz $ handleKey mods key tz

handleEvent tz _ = continue tz

main :: IO ()
main = getArgs >>= \case
  path : _ -> editFile path
  []       -> editBuffer

editFile :: FilePath -> IO ()
editFile path = do
  contents <- Text.strip <$> Text.readFile path
  assemble <$> Brick.defaultMain app (expose contents)
    >>= maybe (fail "invalid text???") (Text.writeFile path)

editBuffer :: IO ()
editBuffer = void $ Brick.defaultMain app (expose [[1..5], [10..12]] :: Exposed [[Int]])
