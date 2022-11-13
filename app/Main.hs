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

import Server.GetMessage qualified as GetMessage
import Server.ListTag    qualified as ListTag
import Server.Save       qualified as Save
import Server.ToggleLog  qualified as ToggleLog

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
  -- init mutable shared state
  verboseVar <- newVerboseVar

  -- init interfaces (plug in state or interfaces where needed)
  ilog  <- initLog verboseVar
  idb   <- initDb config.db
  itime <- initTime config.time
  let
    isetup = initSetup verboseVar

    -- init local envirnoments
    env =
      Env
        { save       = Save.Env (addLogContext "api.save" ilog) idb.save itime
        , getMessage = GetMessage.Env idb.getMessage (addLogContext "api.get-message" ilog)
        , listTag    = ListTag.Env idb.listTag (addLogContext "api.list-tag" ilog)
        , toggleLogs = ToggleLog.Env (addLogContext "api.toggle-log" ilog) isetup
        }

  ilog.logInfo $ "Start server on http://localhost:" <> display config.port
  run config.port $ serve (Proxy :: Proxy Api) (server env)
