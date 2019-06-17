{-# LANGUAGE OverloadedStrings #-}

module MusicServer.Server where

import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Web.Scotty (ActionM, json, liftAndCatchIO, get, scotty)
import MusicServer.Library (Library, lAllTracks)
import MusicServer.JSON ()
import Control.Monad.Trans.Class (lift)
import Control.Monad (liftM)

data AppEnv l = AppEnv { aeLibrary :: l }
type ServerM l a = ReaderT (AppEnv l) ActionM a

serverLiftAndCatchIO :: IO a -> ServerM l a
serverLiftAndCatchIO = (lift . liftAndCatchIO)

sendAllTracks :: Library l => ServerM l ()
sendAllTracks = do
  ioTracks <- asks (lAllTracks . aeLibrary)
  tracks <- serverLiftAndCatchIO ioTracks
  lift $ json tracks

runServer :: Library l => AppEnv l -> IO ()
runServer env = let
  sendAllTracksAction = runReaderT sendAllTracks env
  in scotty 3000 $ do
    get "/library/tracks/_all" sendAllTracksAction
