-- | Toggle logs handler
module Server.ToggleLog
  ( Env(..)
  , handle
  ) where

import DI.Log
import Types

data Env = Env
  { log :: LogVar
  }

-----------------------------------------
-- Handler

handle :: App Env ()
handle = do
  Log{..} <- askLog

  env <- ask
  liftIO $ do
    logInfo "toggle log call"
    toggleLog env.log
