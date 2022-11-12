module Server
  ( server
  , Env(..)
  , Db(..)
  , module X
  ) where

import Data.ByteString.Lazy   qualified as BL
import Data.Text.Encoding     qualified as Text
import DI.Log
import DI.Time
import Servant
import Server.GetById    qualified as GetById
import Server.GetByTag   qualified as GetByTag
import Server.Save       qualified as Save
import Server.ToggleLog  qualified as ToggleLog

import Api as X
import Types (App, runApp, liftIO)

-- | Service environement
data Env = Env
  { log  :: LogVar
  , db   :: Db
  , time :: Time
  }

-- | All DB interfaces by method
data Db = Db
  { save     :: Save.Db
  , getById  :: GetById.Db
  , getByTag :: GetByTag.Db
  }

server :: Env -> Server Api
server env =
       onRequest1 saveEnv Save.handle
  :<|> onRequest1 getByIdEnv GetById.handle
  :<|> onRequest1 getByTagEnv GetByTag.handle
  :<|> onRequest  toggleLogEnv ToggleLog.handle
  where
    saveEnv =
      Save.Env
        { db = env.db.save
        , time = env.time
        , log = addContext "api.save" env.log
        }

    getByIdEnv =
      GetById.Env
        { db = env.db.getById
        , log = addContext "api.get-message" env.log
        }

    getByTagEnv =
      GetByTag.Env
        { db = env.db.getByTag
        , log = addContext "api.get-tag" env.log
        }

    toggleLogEnv =
      ToggleLog.Env
        { log = addContext "api.toggle-log" env.log
        }

    addContext namespace = mapLog (addLogContext namespace)

onRequest :: env -> App env resp -> Servant.Handler resp
onRequest e handler = do
  eResp <- liftIO (runApp handler e)
  either toServantError pure eResp
  where
    toServantError err = throwError $ err400 { errBody = BL.fromStrict $ Text.encodeUtf8 err }

onRequest1 :: env -> (req -> App env resp) -> req -> Servant.Handler resp
onRequest1 env handle a = onRequest env (handle a)

