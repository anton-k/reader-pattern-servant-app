-- | Logger interface and utilities
module DI.Log
  ( HasLog
  , Log(..)
  , askLog
  , LogVar(..)
  , readLogVar
  , toggleLog
  , display
  , mapLog
  , addLogContext
  ) where

import Data.Text qualified as Text
import Control.Monad.Reader as X
import Control.Concurrent.STM
import Types

type HasLog  env = HasField "log" env LogVar

askLog :: HasLog env => App env Log
askLog = liftIO . readLogVar =<< asks (.log)

data Log = Log
  { logInfo  :: Text -> IO ()
  , logDebug :: Text -> IO ()
  , logError :: Text -> IO ()
  }

data LogVar = LogVar
  { isVerboseLog :: TVar Bool
  , log          :: Log
  }

toggleLog :: LogVar -> IO ()
toggleLog var = atomically $ modifyTVar' var.isVerboseLog not

readLogVar :: LogVar -> IO Log
readLogVar var = do
  isVerbose <- readTVarIO var.isVerboseLog
  pure $ if isVerbose
    then var.log
    else silentLog

silentLog :: Log
silentLog = Log
  { logInfo = silent
  , logDebug = silent
  , logError = silent
  }
  where
    silent = const $ pure ()

mapLog :: (Log -> Log) -> LogVar -> LogVar
mapLog f var = var { log = f var.log }

addLogContext :: Text -> Log -> Log
addLogContext contextMesage =
  middleLog (mappend (contextMesage <> ": "))

middleLog :: (Text -> Text) -> Log -> Log
middleLog go logger = Log
  { logInfo = logger.logInfo . go
  , logDebug = logger.logDebug . go
  , logError = logger.logError . go
  }

display :: Show a => a -> Text
display = Text.pack . show
