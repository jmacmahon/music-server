module MusicServer.Library.Generic where

import Data.Text (Text)
import MusicServer.Library.Metadata (Track, mdAlbum, mdArtist, tMetadata)

class Library l where
  lAllTracks :: l -> IO [Track]
  lAlbumQuery :: Text -> l -> IO [Track]
  lAlbumQuery album library = let
      pred :: Track -> Bool
      pred = (== album) . mdAlbum . tMetadata
    in do
      allTracks <- lAllTracks library
      return $ filter pred allTracks
  lArtistQuery :: Text -> l -> IO [Track]
  lArtistQuery artist library = let
      pred :: Track -> Bool
      pred = (== artist) . mdArtist . tMetadata
    in do
      allTracks <- lAllTracks library
      return $ filter pred allTracks