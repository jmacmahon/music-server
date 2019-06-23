{-# LANGUAGE OverloadedStrings #-}

module MusicServer.Server where

import Control.Monad.Trans.Reader (runReaderT)
import Web.Scotty (scotty)
import MusicServer.Library.Generic (Library)
import MusicServer.Server.Environment (AppEnv)
import MusicServer.Server.Routes (routes)

runServer :: Library l => AppEnv l -> IO ()
runServer env = scotty 3000 $ runReaderT routes env
