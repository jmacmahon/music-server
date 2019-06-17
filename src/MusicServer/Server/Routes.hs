{-# LANGUAGE OverloadedStrings #-}

module MusicServer.Server.Routes where

import Control.Monad.Trans.Reader (asks)
import Web.Scotty (json)
import MusicServer.Library (Library, lAllTracks)
import MusicServer.JSON ()
import Control.Monad.Trans.Class (lift)
import MusicServer.Server.Environment (ServerActionM, ServerScottyM, serverLiftAndCatchIO, get_, aeLibrary)

sendAllTracks :: Library l => ServerActionM l ()
sendAllTracks = do
  ioTracks <- asks (lAllTracks . aeLibrary)
  tracks <- serverLiftAndCatchIO ioTracks
  lift $ json tracks

routes :: Library l => ServerScottyM l ()
routes = do
  get_ "/library/tracks/_all" sendAllTracks