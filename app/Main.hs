module Main where

import MusicServer.Server (runServer, AppEnv (AppEnv))
import MusicServer.Library (buildListLibrary)

main :: IO ()
main = let
  env = AppEnv (buildListLibrary "/home/joe/Music")
  in runServer env
