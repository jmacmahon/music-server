{-# LANGUAGE OverloadedStrings #-}

module MusicServer.JSON where

import MusicServer.Library (Metadata, Track, mdTitle, mdArtist, mdAlbum, tMetadata, tFilepath)
import Data.Aeson (ToJSON, toJSON, object, (.=))

instance ToJSON Metadata where
  toJSON md = object [
      "title" .= (mdTitle md),
      "artist" .= (mdArtist md),
      "album" .= (mdAlbum md)
    ]

instance ToJSON Track where
  toJSON tr = object [
      "metadata" .= (tMetadata tr),
      "filepath" .= (tFilepath tr)
    ]