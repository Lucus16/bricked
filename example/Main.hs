{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad ((>=>), void)
import Cursor.List
import Cursor.Text
import Cursor.TextField
import Cursor.Types qualified as Cursor
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Zipper (TextZipper, deleteChar, deletePrevChar, gotoBOL, gotoEOL, insertChar, moveDown, moveLeft, moveRight, moveUp, stringZipper, textZipper)
import Data.Text.Zipper qualified as TextZipper
import Data.Time.Clock (UTCTime)
import Data.Tuple (swap)
import Data.Word (Word64)
import System.Directory ()
import System.Environment (getArgs)
import System.Exit ()
import Text.Read (readMaybe)

import Brick (App(..), Location(..), Widget, continue, halt)
import Brick qualified as Brick
import Brick.Main qualified as Brick
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Brick.Widgets.Core qualified as Brick

import Graphics.Vty.Attributes qualified as Vty
import Graphics.Vty.Input.Events qualified as Vty

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

instance Editable Text where
  newtype Exposed Text = ExposedText { unExposedText :: TextZipper Text }
  blank = expose ("" :: Text)
  expose t = ExposedText $ textZipper [t] Nothing
  suggest = const []
  assemble = Just . Text.concat . TextZipper.getText . unExposedText

  handleKey []          (Vty.KChar c)   = Just . ExposedText . insertChar c . unExposedText
  handleKey [Vty.MCtrl] (Vty.KChar 'a') = Just . ExposedText . gotoBOL . unExposedText
  handleKey [Vty.MCtrl] (Vty.KChar 'e') = Just . ExposedText . gotoEOL . unExposedText

  handleKey [] Vty.KLeft  = Just . ExposedText . TextZipper.moveLeft . unExposedText
  handleKey [] Vty.KRight = Just . ExposedText . TextZipper.moveRight . unExposedText
  handleKey [] Vty.KUp    = Just . ExposedText . TextZipper.moveUp . unExposedText
  handleKey [] Vty.KDown  = Just . ExposedText . TextZipper.moveDown . unExposedText
  handleKey [] Vty.KHome  = Just . ExposedText . TextZipper.gotoBOL . unExposedText
  handleKey [] Vty.KEnd   = Just . ExposedText . TextZipper.gotoEOL . unExposedText
  handleKey [] Vty.KBS    = Just . ExposedText . TextZipper.deletePrevChar . unExposedText
  handleKey [] Vty.KDel   = Just . ExposedText . TextZipper.deleteChar . unExposedText
  handleKey [] Vty.KEnter = Just . ExposedText . TextZipper.breakLine . unExposedText
  handleKey _ _           = const Nothing

  --Vty.KLeft   -> mDo textFieldCursorSelectPrevChar
  --Vty.KRight  -> mDo textFieldCursorSelectNextChar
  --Vty.KUp     -> mDo textFieldCursorSelectPrevLine
  --Vty.KDown   -> mDo textFieldCursorSelectNextLine
  --Vty.KBS     -> mDo (textFieldCursorRemove >=> Cursor.dullDelete)
  --Vty.KDel    -> mDo (textFieldCursorDelete >=> Cursor.dullDelete)
  --Vty.KChar c -> continue . fromMaybe tc $ textFieldCursorInsertChar c $ Just tc

class Drawable a where
  draw :: a -> [Widget Text]

instance Drawable (TextZipper Text) where
  draw tz = pure
    $ Brick.showCursor "cursor" (Location $ swap $ TextZipper.cursorPosition tz)
    $ Brick.txtWrap (Text.concat $ map (<>"\n") $ TextZipper.getText tz)

instance Drawable (Exposed Text) where
  draw = draw . unExposedText

exposeShow :: Show a => a -> TextZipper String
exposeShow i = stringZipper [show i] (Just 1)

assembleRead :: Read a => TextZipper String -> Maybe a
assembleRead = readMaybe . concat . TextZipper.getText

newtype ReadShowEditable a = ReadShowEditable a
  deriving newtype (Read, Show)

instance (Read a, Show a) => Editable (ReadShowEditable a) where
  newtype Exposed (ReadShowEditable a) = ExposedByReadShow { unExposedByReadShow :: TextZipper String }
  blank = ExposedByReadShow $ stringZipper [""] (Just 1)
  expose i = ExposedByReadShow $ stringZipper [show i] (Just 1)
  suggest = const []
  assemble = readMaybe . concat . TextZipper.getText . unExposedByReadShow

  handleKey []          (Vty.KChar c)   = Just . ExposedByReadShow . insertChar c . unExposedByReadShow
  handleKey [Vty.MCtrl] (Vty.KChar 'a') = Just . ExposedByReadShow . gotoBOL . unExposedByReadShow
  handleKey [Vty.MCtrl] (Vty.KChar 'e') = Just . ExposedByReadShow . gotoEOL . unExposedByReadShow

  handleKey [] Vty.KLeft  = Just . ExposedByReadShow . moveLeft . unExposedByReadShow
  handleKey [] Vty.KRight = Just . ExposedByReadShow . moveRight . unExposedByReadShow
  handleKey [] Vty.KUp    = Just . ExposedByReadShow . moveUp . unExposedByReadShow
  handleKey [] Vty.KDown  = Just . ExposedByReadShow . moveDown . unExposedByReadShow
  handleKey [] Vty.KBS    = Just . ExposedByReadShow . deletePrevChar . unExposedByReadShow
  handleKey [] Vty.KDel   = Just . ExposedByReadShow . deleteChar . unExposedByReadShow
  handleKey _ _           = const Nothing

--deriving via (ReadShowEditable Int) instance Editable Int

instance Editable Int where
  newtype Exposed Int = ExposedInt { unExposedInt :: TextZipper String }
  blank = expose (0 :: Int)
  expose = ExposedInt . exposeShow
  assemble = assembleRead . unExposedInt

  handleKey [] Vty.KLeft     = Just . ExposedInt . moveLeft . unExposedInt
  handleKey [] Vty.KRight    = Just . ExposedInt . moveRight . unExposedInt
  handleKey [] Vty.KBS       = Just . ExposedInt . deletePrevChar . unExposedInt
  handleKey [] Vty.KDel      = Just . ExposedInt . deleteChar . unExposedInt
  handleKey [] (Vty.KChar c) = Just . ExposedInt . insertChar c . unExposedInt
  handleKey _ _              = const Nothing

instance Editable Word64 where
  newtype Exposed Word64 = ExposedWord64 { unExposedWord64 :: TextZipper String }
  blank = expose (0 :: Word64)
  expose = ExposedWord64 . exposeShow
  assemble = assembleRead . unExposedWord64

data Role
  = User
  | Admin
  deriving (Read, Show)

instance Editable Role where
  newtype Exposed Role = ExposedRole { unExposedRole :: TextZipper String }
  blank = ExposedRole $ stringZipper [] Nothing
  expose = ExposedRole . exposeShow
  suggest = const $ map expose [Admin, User]
  assemble = assembleRead . unExposedRole

instance Editable a => Editable [a] where
  newtype Exposed [a] = ExposedList { unExposedList :: ListCursor (Node a) }
  blank = ExposedList emptyListCursor
  expose = ExposedList . makeListCursor . map Complete
  assemble = traverse assembleNode . rebuildListCursor . unExposedList

instance Editable UTCTime where
  newtype Exposed UTCTime = ExposedUTCTime { unExposedUTCTime :: TextZipper String }
  blank = ExposedUTCTime $ stringZipper [] Nothing
  expose = ExposedUTCTime . exposeShow
  assemble = assembleRead . unExposedUTCTime

data Node a
  = Exposed !(Exposed a)
  | Complete !a

pack :: Editable a => Exposed a -> Node a
pack x = maybe (Exposed x) Complete $ assemble x

unpack :: Editable a => Node a -> Exposed a
unpack (Exposed x) = x
unpack (Complete x) = expose x

assembleNode :: Editable a => Node a -> Maybe a
assembleNode (Exposed x) = assemble x
assembleNode (Complete x) = Just x

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

app :: Brick.App (Exposed Text) e Text
app = Brick.App
  { appDraw = draw
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const $ Brick.attrMap Vty.defAttr []
  }

handleEvent :: Exposed Text -> Brick.BrickEvent Text e -> Brick.EventM Text (Brick.Next (Exposed Text))
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
editBuffer = do
  assemble <$> Brick.defaultMain app blank
    >>= maybe (fail "invalid text???") (Text.putStrLn)
