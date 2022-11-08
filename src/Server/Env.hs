module Server.Env
  ( Env(..)
  , IDb(..)
  , ITime(..)
  , ILog(..)
  , ILogVar(..)
  , readLogVar
  , toggleLog
  , display
  , mapILog
  , addLogContext
  ) where

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Text qualified as Text
import Control.Concurrent.STM
import Types

data Env = Env
  { log  :: ILogVar
  , db   :: IDb
  , time :: ITime
  }

-- DB / storage
--
-- it's interesting to consider IWriteDb and IReadDb as some methods are read only

data IDb = IDb
  { saveMessage :: Message -> IO MessageId
  , getMessage  :: MessageId -> IO (Maybe Message)
  , getByTag    :: Tag -> IO [Message]
  }

-- Times

data ITime = ITime
  { now :: IO UTCTime
  }

-- Logs

data ILog = ILog
  { logInfo  :: Text -> IO ()
  , logDebug :: Text -> IO ()
  , logError :: Text -> IO ()
  }

data ILogVar = ILogVar
  { isVerboseLog :: TVar Bool
  , log          :: ILog
  }

toggleLog :: ILogVar -> IO ()
toggleLog var = atomically $ modifyTVar' var.isVerboseLog not

readLogVar :: ILogVar -> IO ILog
readLogVar var = do
  isVerbose <- readTVarIO var.isVerboseLog
  pure $ if isVerbose
    then var.log
    else silentLog


silentLog :: ILog
silentLog = ILog
  { logInfo = silent
  , logDebug = silent
  , logError = silent
  }
  where
    silent = const $ pure ()

mapILog :: (ILog -> ILog) -> ILogVar -> ILogVar
mapILog f var = ILogVar { isVerboseLog = var.isVerboseLog, log = f var.log }

addLogContext :: Text -> ILog -> ILog
addLogContext contextMesage =
  middleLog (mappend (contextMesage <> ": "))

middleLog :: (Text -> Text) -> ILog -> ILog
middleLog go logger = ILog
  { logInfo = logger.logInfo . go
  , logDebug = logger.logDebug . go
  , logError = logger.logError . go
  }

display :: Show a => a -> Text
display = Text.pack . show
