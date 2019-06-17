module MusicServer.Library where

import System.FilePath.Find (find, always, fileType, FileType (RegularFile, SymbolicLink), (==?), (||?))
import Data.Maybe (catMaybes)
import Sound.HTagLib (TagGetter, getTags, HTagLibException, titleGetter, unTitle, artistGetter, unArtist, albumGetter, unAlbum)
import Data.Text (Text)
import Control.Exception (IOException, catch)

data Metadata = Metadata {
  mdTitle :: Text,
  mdArtist :: Text,
  mdAlbum :: Text
} deriving (Show)

data Track = Track {
  tMetadata :: Metadata,
  tFilepath :: FilePath
} deriving (Show)

class Library l where
  allTracks :: l -> IO [Track]

data ListLibrary = ListLibrary { llList :: IO [Track] }
instance Library ListLibrary where
  allTracks = llList

buildListLibrary :: FilePath -> ListLibrary
buildListLibrary root = ListLibrary $ do
  files <- findFiles root
  tracks <- catMaybes <$> mapM parseTrack files
  return tracks

findFiles :: FilePath -> IO [FilePath]
findFiles = find always (fileType ==? RegularFile ||? fileType ==? SymbolicLink)

parseTrack :: FilePath -> IO (Maybe Track)
parseTrack filePath = let
  unsafeJust :: IO (Maybe Track)
  unsafeJust = Just <$> unsafeParseTrack filePath
  nothingHandler :: HTagLibException -> IO (Maybe Track)
  nothingHandler = const $ return Nothing
  in catch unsafeJust nothingHandler

unsafeParseTrack :: FilePath -> IO Track
unsafeParseTrack filePath = do
  metadata <- getTags filePath metadataGetter
  return $ Track metadata filePath

metadataGetter :: TagGetter Metadata
metadataGetter = Metadata <$> (unTitle <$> titleGetter) <*> (unArtist <$> artistGetter) <*> (unAlbum <$> albumGetter)
