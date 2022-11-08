-- | Init logs
module DI.Log
  ( initLogVar
  ) where

import Control.Concurrent.STM
import Data.Text.IO qualified as Text
import Server.Env

initLogVar :: IO ILogVar
initLogVar = ILogVar <$> initVerbosity <*> initLog

initVerbosity :: IO (TVar Bool)
initVerbosity = newTVarIO True

initLog :: IO ILog
initLog = do
  pure $ ILog
    { logInfo = logConsole "INFO"
    , logError = logConsole "ERROR"
    , logDebug = logConsole "DEBUG"
    }
  where
    logConsole tag txt = Text.putStrLn $ mconcat ["[", tag, "]: ", txt ]
