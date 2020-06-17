{-# LANGUAGE OverloadedStrings #-}

module MusicServer.Library.Generic where

import Data.Text (Text)
import MusicServer.Library.Metadata (Track, Album (Album), mdAlbum, mdArtist, tMetadata)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromMaybe)

class Library l where
  lAllTracks :: l -> IO [Track]
  lAllAlbums :: l -> IO [Album]
  lAllAlbums library = let
      hasSameAlbum :: Track -> Track -> Bool
      hasSameAlbum t1 t2 = (mdAlbum . tMetadata) t1 == (mdAlbum . tMetadata) t2
      trackGroupToAlbum :: [Track] -> Album
      trackGroupToAlbum group = let
          headTrack = head group
          albumTitle = (mdAlbum . tMetadata) headTrack
          allArtists = map (mdArtist . tMetadata) group
          albumArtist = fromMaybe "Various Artists" $ allSame allArtists
        in Album albumTitle albumArtist
    in do
      allTracks <- lAllTracks library
      let sortedTracks = sortOn (mdAlbum . tMetadata) allTracks
      let groupedTracks = groupBy hasSameAlbum sortedTracks
      return $ map trackGroupToAlbum groupedTracks
  lAlbumQuery :: Text -> l -> IO [Track]
  lAlbumQuery album library = let
      matchesAlbum :: Track -> Bool
      matchesAlbum = (== album) . mdAlbum . tMetadata
    in do
      allTracks <- lAllTracks library
      return $ filter matchesAlbum allTracks
  lArtistQuery :: Text -> l -> IO [Track]
  lArtistQuery artist library = let
      pred :: Track -> Bool
      pred = (== artist) . mdArtist . tMetadata
    in do
      allTracks <- lAllTracks library
      return $ filter pred allTracks

allSame :: Eq a => [a] -> Maybe a
allSame [] = Nothing
allSame (v:vs) = let
    folder :: Eq a => Maybe a -> a -> Maybe a
    folder Nothing _ = Nothing
    folder (Just a) b = if a == b
      then Just b
      else Nothing
  in foldl folder (Just v) vs