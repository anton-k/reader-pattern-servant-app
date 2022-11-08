module Main (main) where

import Data.Proxy
import Servant
import Network.Wai.Handler.Warp (run)

import Server
import Types
import DI.Db
import DI.Log
import DI.Time

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
  env <-
    Env <$> initLogVar
        <*> initDb config.db
        <*> initTime config.time

  ILog{..} <- readLogVar env.log
  logInfo $ "Start server on http://localhost:" <> display config.port
  liftIO $ run config.port $ serve (Proxy :: Proxy Api) (server env)
