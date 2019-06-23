{-# LANGUAGE OverloadedStrings #-}

module MusicServer.Server.Routes where

import Control.Monad.Trans.Reader (asks)
import Web.Scotty (json, param)
import MusicServer.Library.Generic (Library, lAllTracks, lAlbumQuery, lArtistQuery)
import MusicServer.JSON ()
import Control.Monad.Trans.Class (lift)
import MusicServer.Server.Environment (ServerActionM, ServerScottyM, serverLiftAndCatchIO, get_, aeLibrary)

getAllTracks :: Library l => ServerActionM l ()
getAllTracks = do
  ioTracks <- asks (lAllTracks . aeLibrary)
  tracks <- serverLiftAndCatchIO ioTracks
  lift $ json tracks

getAlbumTracks :: Library l => ServerActionM l ()
getAlbumTracks = do
  album <- lift $ param "albumName"
  ioTracks <- asks ((lAlbumQuery album) . aeLibrary)
  tracks <- serverLiftAndCatchIO ioTracks
  lift $ json tracks

getArtistTracks :: Library l => ServerActionM l ()
getArtistTracks = do
  artist <- lift $ param "artistName"
  ioTracks <- asks ((lArtistQuery artist) . aeLibrary)
  tracks <- serverLiftAndCatchIO ioTracks
  lift $ json tracks

routes :: Library l => ServerScottyM l ()
routes = do
  get_ "/library/tracks/" getAllTracks
  get_ "/library/album/:albumName" getAlbumTracks
  get_ "/library/artist/:artistName" getArtistTracks