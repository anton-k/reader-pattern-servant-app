module Server.Save
  ( SaveEnv(..)
  , handleSave
  ) where

import Server.Class
import Server.Env

import Api
import Types

data SaveEnv = SaveEnv
  { log   :: ILogVar
  , db    :: IDb
  , time  :: ITime
  }

instance HasLog SaveEnv where
  getLog env = env.log

instance HasDb SaveEnv where
  getDb env = env.db

instance HasTime SaveEnv where
  getTime env = env.time

-----------------------------------------
-- Handler

handleSave :: SaveRequest -> App SaveEnv SaveResponse
handleSave req = do
  ITime{..} <- askTime
  IDb{..}   <- askDb
  ILog{..}  <- askLog

  liftIO $ do
    logInfo $ "save call: " <> display req
    time <- now
    let msg = Message req.message req.tags time
    logInfo $ "create message: " <> display msg
    SaveResponse <$> saveMessage msg

