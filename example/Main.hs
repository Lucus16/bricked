{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bricked.Expr
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, modify, runStateT)
import Data.Aeson qualified as Aeson
import Graphics.Vty
import Graphics.Vty.Platform.Unix
import System.Environment (getArgs)
import System.IO (hPrint, stderr)

foo :: SelJson
foo =
  Fix $
    Selected $
      Fix $
        Array $
          Fix
            <$> [ Null
                , Bool False
                , Bool True
                , Number "3.14"
                , String "foo"
                , Object $
                    Fix
                      <$> [ Comment "wow"
                          , Comment "nice"
                          , KeyValue (Fix $ String "pos") $ Fix $ Array []
                          , KeyValue (Fix $ String "description") $ Fix $ String "multiline\nstring"
                          ]
                ]

main :: IO ()
main = do
  vty <- mkVty defaultConfig
  initialContents <-
    getArgs >>= \case
      [] -> pure foo
      path : _ -> Aeson.eitherDecodeFileStrict' path >>= either fail (pure . Fix . Selected . Fix)
  void $ flip runStateT initialContents do
    while (not <$> liftIO (isShutdown vty)) do
      contents <- get
      liftIO $
        update vty $
          picForImage $
            render 80 contents
              <-> backgroundFill 1 2
              <-> debug contents
      liftIO (nextEvent vty) >>= \case
        EvKey (KChar 'q') [] -> liftIO $ shutdown vty
        EvKey (KChar 'G') [] -> modify select
        EvKey (KChar 'g') [] -> modify select
        EvKey (KChar 'k') [] -> modify jsonSelectKeys
        EvKey KLeft [] -> modify selectAllParents
        EvKey KRight [] -> modify selectAllChildren
        EvKey KUp [] -> modify selectPrevOrNone
        EvKey KDown [] -> modify selectNextOrNone
        _ -> pure ()

while :: (Monad m) => m Bool -> m () -> m ()
while cond body = cond >>= flip when (body >> while cond body)
