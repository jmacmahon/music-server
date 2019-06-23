module MusicServer.Library.Metadata where

import Data.Text (Text)

data Metadata = Metadata {
  mdTitle :: Text,
  mdArtist :: Text,
  mdAlbum :: Text,
  mdTrackNumber :: Maybe Int
} deriving (Show)

data Track = Track {
  tMetadata :: Metadata,
  tFilepath :: FilePath
} deriving (Show)