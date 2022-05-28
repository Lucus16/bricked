{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad ((>=>))
import Cursor.List
import Cursor.Text
import Cursor.TextField
import Cursor.Types qualified as Cursor
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Zipper (TextZipper, stringZipper, textZipper)
import Data.Text.Zipper qualified as TextZipper
import Data.Time.Clock (UTCTime)
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

data Role
  = Admin
  | User
  deriving (Read, Show)

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

class Editable a where
  type Exposed a
  zero :: Exposed a
  expose :: a -> Exposed a
  suggest :: Exposed a -> [Exposed a]
  suggest = const []
  assemble :: Exposed a -> Maybe a

instance Editable Text where
  type Exposed Text = TextZipper Text
  zero = expose ("" :: Text)
  expose t = textZipper [t] Nothing
  suggest = const []
  assemble = Just . Text.concat . TextZipper.getText

exposeShow :: Show a => a -> TextZipper String
exposeShow i = stringZipper [show i] (Just 1)

assembleRead :: Read a => TextZipper String -> Maybe a
assembleRead = readMaybe . concat . TextZipper.getText

instance Editable Int where
  type Exposed Int = TextZipper String
  zero = expose (0 :: Int)
  expose = exposeShow
  assemble = assembleRead

instance Editable Role where
  type Exposed Role = TextZipper String
  zero = stringZipper [] Nothing
  expose = exposeShow
  suggest = const $ map expose [Admin, User]
  assemble = assembleRead

instance Editable a => Editable [a] where
  type Exposed [a] = ListCursor (Node a)
  zero = emptyListCursor
  expose = makeListCursor . map Complete
  assemble = traverse assembleNode . rebuildListCursor

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

data EditingBefore f
  = Constructor f
  | forall i. BeforeField (EditingBefore (i -> f)) (Node i)

data EditingAfter a o
  = Finisher (a -> o)
  | forall i. AfterField (Node i) (EditingAfter (i -> o) o)

data EditingRecord o = forall a i. EditingRecord
  { erBefore  :: EditingBefore (i -> a)
  , erCurrent :: Exposed i
  , erAfter   :: EditingAfter a o
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

instance Editable Session where
  type Exposed Session = EditingRecord Session
  expose session = EditingRecord
    { erBefore = Constructor Session :: EditingBefore (Text -> UTCTime -> Session)
    , erCurrent = sessionSecret session :: Exposed Text
    , erAfter = AfterField (pack (sessionExpiresAt)) (Finisher id) :: EditingAfter (UTCTime -> Session) Session
    }

  assemble er = do
    pure $ Session
      { sessionSecret = undefined
      , sessionExpiresAt = undefined
      }

--instance Editable Role where
--  edit = options
--    [ atom "Admin" Admin
--    , atom "User" User
--    ]
--
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

app :: Brick.App TextFieldCursor e Text
app = Brick.App
  { appDraw = draw
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const $ Brick.attrMap Vty.defAttr []
  }

draw :: TextFieldCursor -> [Widget Text]
draw tc = pure
  $ Brick.showCursor "cursor" (Location (x, y))
  $ Brick.txtWrap (rebuildTextFieldCursor tc)
  where
    (y, x) = textFieldCursorSelection tc

handleEvent :: TextFieldCursor -> Brick.BrickEvent Text e -> Brick.EventM Text (Brick.Next TextFieldCursor)
handleEvent tc (Brick.VtyEvent (Vty.EvKey key mods)) = case key of
  Vty.KLeft   -> mDo textFieldCursorSelectPrevChar
  Vty.KRight  -> mDo textFieldCursorSelectNextChar
  Vty.KUp     -> mDo textFieldCursorSelectPrevLine
  Vty.KDown   -> mDo textFieldCursorSelectNextLine
  Vty.KEsc    -> halt tc
  Vty.KEnter  -> halt tc
  Vty.KBS     -> mDo (textFieldCursorRemove >=> Cursor.dullDelete)
  Vty.KDel    -> mDo (textFieldCursorDelete >=> Cursor.dullDelete)
  Vty.KChar c -> continue . fromMaybe tc $ textFieldCursorInsertChar c $ Just tc
  _      -> continue tc
  where
    mDo f = continue . fromMaybe tc $ f tc
handleEvent tc _ = continue tc

main :: IO ()
main = do
  path : _ <- getArgs
  contents <- Text.strip <$> Text.readFile path
  Brick.defaultMain app (makeTextFieldCursor contents)
    >>= Text.writeFile path . (<>"\n") . Text.strip . rebuildTextFieldCursor
