module MusicServer.Server.Environment where

import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Web.Scotty (ActionM, ScottyM, RoutePattern, liftAndCatchIO, get)
import MusicServer.Library.Generic (Library)
import Control.Monad.Trans.Class (lift)

data AppEnv l = AppEnv { aeLibrary :: l }
type ServerActionM l a = ReaderT (AppEnv l) ActionM a
type ServerScottyM l a = ReaderT (AppEnv l) ScottyM a

serverLiftAndCatchIO :: IO a -> ServerActionM l a
serverLiftAndCatchIO = (lift . liftAndCatchIO)

get_ :: Library l => RoutePattern -> ServerActionM l () -> ServerScottyM l ()
get_ pattern action = do
  env <- ask
  let actionM = runReaderT action env
  let scottyM = get pattern actionM
  lift scottyM
