module MusicServer.Library.ListLibrary where

import System.FilePath.Find (find, always, fileType, FileType (RegularFile, SymbolicLink), (==?), (||?))
import Data.Maybe (catMaybes)
import Sound.HTagLib (TagGetter, getTags, HTagLibException, titleGetter, unTitle, artistGetter, unArtist, albumGetter, unAlbum, trackNumberGetter, unTrackNumber)
import Data.Text (Text)
import Control.Exception (IOException, catch)
import MusicServer.Library.Generic (Library, lAllTracks)
import MusicServer.Library.Metadata (Track (Track), Metadata (Metadata))

data ListLibrary = ListLibrary { llList :: IO [Track] }
instance Library ListLibrary where
  lAllTracks = llList

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
metadataGetter = Metadata <$>
    (unTitle <$> titleGetter) <*>
    (unArtist <$> artistGetter) <*>
    (unAlbum <$> albumGetter) <*>
    (unTrackNumber <$$> trackNumberGetter)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f v = (f <$>) <$> v