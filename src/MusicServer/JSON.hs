{-# LANGUAGE OverloadedStrings #-}

module MusicServer.JSON where

import MusicServer.Library.Metadata (Metadata, Track, Album, mdTitle, mdArtist, mdAlbum, mdTrackNumber, tMetadata, tFilepath, abTitle, abArtist)
import Data.Aeson (ToJSON, toJSON, object, (.=))

instance ToJSON Metadata where
  toJSON md = object [
      "title" .= (mdTitle md),
      "artist" .= (mdArtist md),
      "album" .= (mdAlbum md),
      "tracknumber" .= (mdTrackNumber md)
    ]

instance ToJSON Track where
  toJSON tr = object [
      "metadata" .= (tMetadata tr),
      "filepath" .= (tFilepath tr)
    ]

instance ToJSON Album where
  toJSON ab = object [
      "title" .= (abTitle ab),
      "artist" .= (abArtist ab)
    ]