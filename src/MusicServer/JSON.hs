{-# LANGUAGE OverloadedStrings #-}

module MusicServer.JSON where

import MusicServer.Library (Metadata, Track, mdTitle, mdArtist, mdAlbum, tMetadata, tFilePath)
import Data.Aeson (ToJSON, toJSON, object, (.=))

instance ToJSON Metadata where
  toJSON md = object ([
      "title" .= (mdTitle md)
    ])