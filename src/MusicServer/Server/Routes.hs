{-# LANGUAGE OverloadedStrings #-}

module MusicServer.Server.Routes where

import Control.Monad.Trans.Reader (asks)
import Web.Scotty (json, param)
import MusicServer.Library (Library, lAllTracks, lAlbumQuery, lArtistQuery)
import MusicServer.JSON ()
import Control.Monad.Trans.Class (lift)
import MusicServer.Server.Environment (ServerActionM, ServerScottyM, serverLiftAndCatchIO, get_, aeLibrary)

getAllTracks :: Library l => ServerActionM l ()
getAllTracks = do
  ioTracks <- asks (lAllTracks . aeLibrary)
  tracks <- serverLiftAndCatchIO ioTracks
  lift $ json tracks

getAlbum :: Library l => ServerActionM l ()
getAlbum = do
  album <- lift $ param "albumName"
  ioTracks <- asks ((lAlbumQuery album) . aeLibrary)
  tracks <- serverLiftAndCatchIO ioTracks
  lift $ json tracks

getArtist :: Library l => ServerActionM l ()
getArtist = do
  artist<- lift $ param "artistName"
  ioTracks <- asks ((lArtistQuery artist) . aeLibrary)
  tracks <- serverLiftAndCatchIO ioTracks
  lift $ json tracks

routes :: Library l => ServerScottyM l ()
routes = do
  get_ "/library/tracks/" getAllTracks
  get_ "/library/album/:albumName" getAlbum
  get_ "/library/artist/:artistName" getArtist