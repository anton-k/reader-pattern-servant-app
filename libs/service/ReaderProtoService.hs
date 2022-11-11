module ReaderProtoService
  ( ReaderProtoService.startLoggingServer,
    Service.Db (..),
    Service.Timer (..),

    -- * Model
    module ApiModel,
  )
where

import ReaderProtoApi as Api
import ReaderProtoApi.Model as ApiModel
import ReaderProtoBase.Prelude
import ReaderProtoService.Service qualified as Service

startLoggingService :: Handle -> Int -> Service.Db -> Service.Timer -> IO Service
startLoggingService loggerHandle verbosity db timer =
  Service.startEnv loggerHandle verbosity db timer <&> \env ->
    let adaptFx :: Service.Fx a -> IO (Either Text a)
        adaptFx = fmap (first adaptErr) . Service.runFx env
     in Service
          { save = adaptFx . Service.save,
            getById = adaptFx . Service.getById,
            getByTag = adaptFx . Service.getByTag,
            toggleLog = adaptFx $ Service.toggleLog
          }
  where
    adaptErr = Service.renderFxErrForPublicAsPlainText

-- |
-- Start a server, which logs the output to the provided handle.
--
-- Completely encapsulates the whole server implementation.
startLoggingServer ::
  -- | Port.
  Int ->
  -- | Output handle for the logger.
  Handle ->
  -- | Initial logger verbosity level.
  Int ->
  -- | Implementation of the DB.
  Service.Db ->
  -- | Implementation of the timer.
  Service.Timer ->
  IO ()
startLoggingServer port loggerHandle loggerVerbosity db timer =
  startLoggingService loggerHandle loggerVerbosity db timer
    >>= Api.serve port
