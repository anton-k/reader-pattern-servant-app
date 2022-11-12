-- | Toggle logs handler
module Server.ToggleLog
  ( Env(..)
  , handle
  ) where

import DI.Log
import DI.Setup
import Types

data Env = Env
  { log   :: Log
  , setup :: Setup
  }

-----------------------------------------
-- Handler

handle :: App Env ()
handle = do
  Log{..} <- askLog
  Setup{..} <- askSetup

  liftIO $ do
    logInfo "toggle log call"
    toggleLogs
