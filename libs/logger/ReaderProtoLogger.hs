module ReaderProtoLogger
  ( Logger,
    acquire,
    inContext,
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
acquire :: Handle -> Int -> IO Logger
acquire handle initialVerbosity = do
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

writeGenerally :: Logger -> Text -> Int -> Text -> IO ()
writeGenerally logger tag verbosity msg = do
  maxVerbosity <- takeMVar logger.maxVerbosityVar
  if verbosity <= maxVerbosity
    then do
      time <- getCurrentTime
      finally
        (logger.write (compiledMessage time))
        (putMVar logger.maxVerbosityVar maxVerbosity)
    else putMVar logger.maxVerbosityVar maxVerbosity
  where
    compiledMessage time =
      if List.null logger.contexts
        then "[" <> tag <> "] " <> fromList (iso8601Show time) <> ": " <> msg
        else "[" <> tag <> "] " <> fromList (iso8601Show time) <> " " <> Text.intercalate "/" (List.reverse logger.contexts) <> ": " <> msg

writeDebug :: Logger -> Text -> IO ()
writeDebug logger =
  writeGenerally logger "DEBUG" 4

writeInfo :: Logger -> Text -> IO ()
writeInfo logger =
  writeGenerally logger "INFO" 3

writeWarn :: Logger -> Text -> IO ()
writeWarn logger =
  writeGenerally logger "WARN" 2

writeError :: Logger -> Text -> IO ()
writeError logger =
  writeGenerally logger "ERROR" 1

setVerbosity :: Logger -> Int -> IO ()
setVerbosity logger level =
  swapMVar logger.maxVerbosityVar level $> ()

getVerbosity :: Logger -> IO Int
getVerbosity logger =
  readMVar logger.maxVerbosityVar
