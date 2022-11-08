module Server
  ( server
  , module X
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as Text
import Servant
import Server.Class as X
import Server.Env as X
import Server.GetById
import Server.GetByTag
import Server.Save
import Server.ToggleLog

import Api as X
import Types (App, runApp)

server :: Env -> Server Api
server env =
       onRequest handleSave saveEnv
  :<|> onRequest handleGetById getByIdEnv
  :<|> onRequest handleGetByTag getByTagEnv
  :<|> onRequest (const handleToggleLog) toggleLogEnv ()
  where
    onRequest :: (req -> App env resp) -> env -> req -> Servant.Handler resp
    onRequest handler e req = do
      eResp <- liftIO $ runApp (handler req) e
      case eResp of
        Right resp -> pure resp
        Left err   -> throwError $ err400 { errBody = BL.fromStrict $ Text.encodeUtf8 err }

    saveEnv    = SaveEnv { log = addContext "api.save" env.log, db = env.db, time = env.time }

    getByIdEnv = GetByIdEnv { db = env.db, log = addContext "api.get-message" env.log }

    getByTagEnv = GetByTagEnv { db = env.db, log = addContext "api.get-tag" env.log }

    toggleLogEnv = ToggleLogEnv { log = addContext "api.toggle-log" env.log }

    addContext namespace = mapILog (addLogContext namespace)

