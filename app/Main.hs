module Main where

import MusicServer.Server (runServer)
import MusicServer.Server.Environment (AppEnv (AppEnv))
import MusicServer.Library.ListLibrary (buildListLibrary)

main :: IO ()
main = let
  env = AppEnv (buildListLibrary "/home/joe/Music")
  in runServer env
