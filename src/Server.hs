module Server
  ( server
  , Env(..)
  , module X
  ) where

import Servant
import Server.GetMessage qualified as GetMessage
import Server.ListTag    qualified as ListTag
import Server.Save       qualified as Save
import Server.ToggleLog  qualified as ToggleLog

import Api as X
import App (toHandler, toHandler1)

-- | Service environment by methods
data Env = Env
  { save        :: Save.Env
  , getMessage  :: GetMessage.Env
  , listTag     :: ListTag.Env
  , toggleLogs  :: ToggleLog.Env
  }

-- | Servant server for the app
server :: Env -> Server Api
server env =
       toHandler1 env.save Save.handle
  :<|> toHandler1 env.getMessage GetMessage.handle
  :<|> toHandler1 env.listTag ListTag.handle
  :<|> toHandler  env.toggleLogs ToggleLog.handle

