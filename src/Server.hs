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
import DI.Setup
import Servant
import Server.GetMessage qualified as GetMessage
import Server.ListTag    qualified as ListTag
import Server.Save       qualified as Save
import Server.ToggleLog  qualified as ToggleLog

import Api as X
import Types (App, runApp, liftIO)

-- | Service environment nterfaces
data Env = Env
  { log   :: Log   -- ^ logger
  , db    :: Db    -- ^ DB
  , time  :: Time  -- ^ current time
  , setup :: Setup -- ^ setup configs
  }

-- | All DB interfaces by method
data Db = Db
  { save        :: Save.Db
  , getMessage :: GetMessage.Db
  , listTag    :: ListTag.Db
  }

-- | Servant server for the app
server :: Env -> Server Api
server env =
       onRequest1 saveEnv Save.handle
  :<|> onRequest1 getMessageEnv GetMessage.handle
  :<|> onRequest1 listTagEnv ListTag.handle
  :<|> onRequest  toggleLogEnv ToggleLog.handle
  where
    saveEnv =
      Save.Env
        { db = env.db.save
        , time = env.time
        , log = addLogContext "api.save" env.log
        }

    getMessageEnv =
      GetMessage.Env
        { db = env.db.getMessage
        , log = addLogContext "api.get-message" env.log
        }

    listTagEnv =
      ListTag.Env
        { db = env.db.listTag
        , log = addLogContext "api.list-tag" env.log
        }

    toggleLogEnv =
      ToggleLog.Env
        { log = addLogContext "api.toggle-log" env.log
        , setup = env.setup
        }

onRequest :: env -> App env resp -> Servant.Handler resp
onRequest e handler = do
  eResp <- liftIO (runApp handler e)
  either toServantError pure eResp
  where
    toServantError err = throwError $ err400 { errBody = BL.fromStrict $ Text.encodeUtf8 err }

onRequest1 :: env -> (req -> App env resp) -> req -> Servant.Handler resp
onRequest1 env handle a = onRequest env (handle a)

