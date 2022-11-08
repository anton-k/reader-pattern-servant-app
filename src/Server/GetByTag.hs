module Server.GetByTag
  ( GetByTagEnv(..)
  , handleGetByTag
  ) where

import Server.Class
import Server.Env
import Types

data GetByTagEnv = GetByTagEnv
  { db  :: IDb
  , log :: ILogVar
  }

instance HasDb GetByTagEnv where
  getDb env = env.db

instance HasLog GetByTagEnv where
  getLog env = env.log

-----------------------------------------
-- Handler

handleGetByTag :: Tag -> App GetByTagEnv [Message]
handleGetByTag tag = do
  IDb{..}  <- askDb
  ILog{..} <- askLog

  liftIO $ do
    logInfo $ "get by tag call: " <> display tag
    getByTag tag
