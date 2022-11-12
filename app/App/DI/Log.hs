-- | Init logs
module App.DI.Log
  ( initLogVar
  ) where

import Control.Concurrent.STM
import Data.Text.IO qualified as Text
import DI.Log

initLogVar :: IO LogVar
initLogVar = LogVar <$> initVerbosity <*> initLog

initVerbosity :: IO (TVar Bool)
initVerbosity = newTVarIO True

initLog :: IO Log
initLog = do
  pure $ Log
    { logInfo = logConsole "INFO"
    , logError = logConsole "ERROR"
    , logDebug = logConsole "DEBUG"
    }
  where
    logConsole tag txt = Text.putStrLn $ mconcat ["[", tag, "]: ", txt ]
