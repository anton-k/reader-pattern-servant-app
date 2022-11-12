module Main (main) where

import Data.Proxy
import Servant
import Network.Wai.Handler.Warp (run)

import Server
import Types
import DI.Log
import App.DI.Db
import App.DI.Log
import App.DI.Setup
import App.DI.Time
import App.State

data Config = Config
  { db   :: Url
  , time :: Url
  , port :: Int
  }

main :: IO ()
main = runServer =<< readConfig

readConfig :: IO Config
readConfig = pure $ Config "path/db" "path/time" 7070

runServer :: Config -> IO ()
runServer config = do
  verboseVar <- newVerboseVar
  env <-
    Env <$> initLog verboseVar
        <*> initDb config.db
        <*> initTime config.time
        <*> pure (initSetup verboseVar)

  env.log.logInfo $ "Start server on http://localhost:" <> display config.port
  run config.port $ serve (Proxy :: Proxy Api) (server env)
