module ReaderProtoLogger
  ( Logger,
    start,

    -- * Modification
    inContext,

    -- * Effects
    runFx,
    Fx,
    writeDebug,
    writeInfo,
    writeWarn,
    writeError,
    setVerbosity,
    getVerbosity,
  )
where

import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import ReaderProtoBase.Prelude

data Logger = Logger
  { maxVerbosityVar :: MVar Int,
    write :: Text -> IO (),
    contexts :: [Text]
  }

-- | Initialize an implementation that logs into the provided system handle.
-- The handle can be a file or an output stream like 'stderr' or 'stdout'.
start :: Handle -> Int -> IO Logger
start handle initialVerbosity = do
  maxVerbosityVar <- newMVar initialVerbosity
  return $ Logger maxVerbosityVar write []
  where
    write msg = do
      TextIO.hPutStrLn handle msg
      hFlush handle

-- | Add a context to the logger.
inContext :: Text -> Logger -> Logger
inContext context logger =
  logger {contexts = context : logger.contexts}

-- * Effects

runFx :: Logger -> Fx a -> IO a
runFx logger fx = runReaderT fx logger

type Fx = ReaderT Logger IO

writeGenerally :: Text -> Int -> Text -> Fx ()
writeGenerally tag verbosity msg = ReaderT $ \logger -> do
  maxVerbosity <- takeMVar logger.maxVerbosityVar
  if verbosity <= maxVerbosity
    then do
      time <- getCurrentTime
      finally
        (logger.write (compiledMessage logger.contexts time))
        (putMVar logger.maxVerbosityVar maxVerbosity)
    else putMVar logger.maxVerbosityVar maxVerbosity
  where
    compiledMessage contexts time =
      if List.null contexts
        then "[" <> tag <> "] " <> fromList (iso8601Show time) <> ": " <> msg
        else "[" <> tag <> "] " <> fromList (iso8601Show time) <> " " <> Text.intercalate "/" (List.reverse contexts) <> ": " <> msg

writeDebug :: Text -> Fx ()
writeDebug = writeGenerally "DEBUG" 4

writeInfo :: Text -> Fx ()
writeInfo = writeGenerally "INFO" 3

writeWarn :: Text -> Fx ()
writeWarn = writeGenerally "WARN" 2

writeError :: Text -> Fx ()
writeError = writeGenerally "ERROR" 1

setVerbosity :: Int -> Fx ()
setVerbosity level =
  ReaderT $ \logger -> swapMVar logger.maxVerbosityVar level $> ()

getVerbosity :: Fx Int
getVerbosity =
  ReaderT $ \logger -> readMVar logger.maxVerbosityVar
