{-# LANGUAGE OverloadedStrings #-}

module MusicServer.Fakes where

import MusicServer.Library (ListLibrary (ListLibrary), Track (Track), Metadata (Metadata))

fakeLibrary :: ListLibrary
fakeLibrary = ListLibrary $ return [
    Track (Metadata "Bampton Fair" "Melrose Quartet" "Fifty Verses" (Just 6)) "/foo/bar/baz.flac",
    Track (Metadata "No track number track" "Melrose Quartet" "Fifty Verses" Nothing) "/foo/bar/baz.flac"
  ]