{-# LANGUAGE OverloadedStrings #-}

module MusicServer.Server.Routes where

import Control.Monad.Trans.Reader (asks)
import Web.Scotty (json, param)
import MusicServer.Library.Generic (Library, lAllTracks, lAllAlbums, lAlbumQuery, lArtistQuery)
import MusicServer.JSON ()
import Control.Monad.Trans.Class (lift)
import MusicServer.Server.Environment (ServerActionM, ServerScottyM, serverLiftAndCatchIO, get_, aeLibrary)
import Data.Aeson (ToJSON)
import Data.Text.Lazy (Text, toStrict)

jsonLibraryGetter :: (Library l, ToJSON j) => (l -> IO j) -> ServerActionM l ()
jsonLibraryGetter getter = do
  ioValue <- asks (getter . aeLibrary)
  value <- serverLiftAndCatchIO ioValue
  lift $ json value

jsonLibraryGetterParam :: (Library l, ToJSON j) => Text -> (Text -> l -> IO j) -> ServerActionM l ()
jsonLibraryGetterParam paramName getter = do
  paramValue <- lift $ param paramName
  ioValue <- asks ((getter paramValue) . aeLibrary)
  value <- serverLiftAndCatchIO ioValue
  lift $ json value

getAllTracks :: Library l => ServerActionM l ()
getAllTracks = jsonLibraryGetter lAllTracks
getAllAlbums :: Library l => ServerActionM l ()
getAllAlbums = jsonLibraryGetter lAllAlbums
getAlbumTracks :: Library l => ServerActionM l () 
getAlbumTracks = jsonLibraryGetterParam "albumName" (lAlbumQuery . toStrict)
getArtistTracks :: Library l => ServerActionM l () 
getArtistTracks = jsonLibraryGetterParam "artistName" (lArtistQuery . toStrict)

routes :: Library l => ServerScottyM l ()
routes = do
  get_ "/library/tracks/" getAllTracks
  get_ "/library/albums/" getAllAlbums
  get_ "/library/album/:albumName" getAlbumTracks
  get_ "/library/artist/:artistName" getArtistTracks