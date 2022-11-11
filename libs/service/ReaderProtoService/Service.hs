module ReaderProtoService.Service
  ( -- * Environment
    Env,

    -- * Errors
    FxErr (..),
    DbErr (..),
    renderFxErrForPublicAsPlainText,

    -- * Management
    startEnv,

    -- ** Injections
    Db (..),
    Timer (..),

    -- * Effects
    runFx,
    Fx,
    save,
    getById,
    getByTag,
    toggleLog,
  )
where

import ReaderProtoApi.Model
import ReaderProtoBase.Prelude
import ReaderProtoLogger qualified as Logger

-- * Environment

-- This env is an example of a composition of open interfaces
-- for dependency injection (db and timer) and a specific implementation (logger).
data Env = Env
  { db :: Db,
    timer :: Timer,
    logger :: Logger.Logger
  }

-- * Errors

data FxErr
  = DbFxErr DbErr
  | MissingMessageFxErr MessageId
  deriving (Show)

data DbErr
  = ConnectionLostDbErr
  | IncompatibilityDbErr Text
  deriving (Show)

renderFxErrForPublicAsPlainText :: FxErr -> Text
renderFxErrForPublicAsPlainText = \case
  DbFxErr _ ->
    "Oops! Server error. Please try again later."
  MissingMessageFxErr messageId ->
    "Message not found by id: " <> showAsText messageId

-- * Management

startEnv :: Handle -> Int -> Db -> Timer -> IO Env
startEnv loggerHandle verbosity db timer =
  Env db timer <$> Logger.acquire loggerHandle verbosity

-- ** Injections

data Db = Db
  { save :: Message -> IO (Either DbErr MessageId),
    getById :: MessageId -> IO (Either DbErr (Maybe Message)),
    getByTag :: Tag -> IO (Either DbErr [Message])
  }

data Timer = Timer
  { now :: IO UTCTime
  }

-- * Effects

runFx :: Env -> Fx a -> IO (Either FxErr a)
runFx env fx =
  runReaderT fx env & runExceptT

type Fx = ReaderT Env (ExceptT FxErr IO)

-- ** Primitives

adaptDbHandler :: (Db -> IO (Either DbErr a)) -> Fx a
adaptDbHandler handler = do
  env <- ask
  resultEither <- liftIO $ handler env.db
  case resultEither of
    Left ConnectionLostDbErr -> do
      logError "DB connection lost"
      throwError $ DbFxErr ConnectionLostDbErr
    Left (IncompatibilityDbErr details) -> do
      logError $ "DB incompatibility error: " <> details
      throwError $ DbFxErr $ IncompatibilityDbErr details
    Right res -> return res

adaptLoggerHandler :: (Logger.Logger -> IO a) -> Fx a
adaptLoggerHandler handler = do
  env <- ask
  liftIO $ handler env.logger

adaptLoggerWritingHandler :: (Logger.Logger -> Text -> IO a) -> Text -> Fx a
adaptLoggerWritingHandler handler msg =
  adaptLoggerHandler $ \logger -> handler logger msg

inLoggingContext :: Text -> Fx a -> Fx a
inLoggingContext context =
  local $ \env ->
    env
      { logger = Logger.inContext context env.logger
      }

logDebug :: Text -> Fx ()
logDebug = adaptLoggerWritingHandler Logger.writeDebug

logInfo :: Text -> Fx ()
logInfo = adaptLoggerWritingHandler Logger.writeInfo

logWarn :: Text -> Fx ()
logWarn = adaptLoggerWritingHandler Logger.writeWarn

logError :: Text -> Fx ()
logError = adaptLoggerWritingHandler Logger.writeError

now :: Fx UTCTime
now = do
  env <- ask
  liftIO $ env.timer.now

-- ** Public Composite Effects

save :: SaveRequest -> Fx MessageId
save request = inLoggingContext "save" $ do
  logInfo $ "Processing request: " <> showAsText request
  time <- now
  let msg = Message request.message request.tags time
  response <- adaptDbHandler $ \db -> db.save msg
  logInfo $ "Responding: " <> showAsText response
  return response

getById :: MessageId -> Fx Message
getById messageId = inLoggingContext "get-by-id" $ do
  logInfo $ "Processing request: " <> showAsText messageId
  responseMaybe <- adaptDbHandler $ \db -> db.getById messageId
  case responseMaybe of
    Nothing -> do
      logInfo $ "Failing"
      throwError $ MissingMessageFxErr messageId
    Just response -> do
      logInfo $ "Responding: " <> showAsText response
      return response

getByTag :: Tag -> Fx [Message]
getByTag request = inLoggingContext "get-by-tag" $ do
  logInfo $ "Processing request: " <> showAsText request
  response <- adaptDbHandler $ \db -> db.getByTag request
  logInfo $ "Responding: " <> showAsText response
  return response

toggleLog :: Fx Bool
toggleLog = inLoggingContext "toggle-log" $ do
  logInfo $ "Processing request"
  priorVerbosity <- adaptLoggerHandler Logger.getVerbosity
  let wasVerbose = priorVerbosity > 0
  if wasVerbose
    then do
      logWarn $ "Disabling verbosity from the following level: " <> showAsText priorVerbosity
      adaptLoggerHandler $ \logger -> Logger.setVerbosity logger 0
    else do
      adaptLoggerHandler $ \logger -> Logger.setVerbosity logger 4
      logInfo $ "Verbosity raised to level " <> showAsText 4
  return wasVerbose
