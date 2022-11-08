module Server.ToggleLog
  ( ToggleLogEnv(..)
  , handleToggleLog
  ) where

import Server.Class
import Server.Env
import Types

data ToggleLogEnv = ToggleLogEnv
  { log :: ILogVar
  }

-----------------------------------------
-- Handler

handleToggleLog :: App ToggleLogEnv ()
handleToggleLog = do
  ILog{..} <- askLog

  env <- ask
  liftIO $ do
    logInfo "toggle log call"
    toggleLog env.log




