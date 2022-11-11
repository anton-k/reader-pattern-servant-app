module ReaderProtoService
  ( startServer,
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

-- |
-- Start the service adapted to the interface expected by the API.
startService :: Handle -> Int -> Service.Db -> Service.Timer -> IO Service
startService loggerHandle verbosity db timer =
  adaptEnv <$> Service.start loggerHandle verbosity db timer
  where
    adaptEnv env =
      Service
        { save = adaptFx . Service.save,
          getById = adaptFx . Service.getById,
          getByTag = adaptFx . Service.getByTag,
          toggleLog = adaptFx $ Service.toggleLog
        }
      where
        adaptFx :: Service.Fx a -> IO (Either Text a)
        adaptFx = fmap (first adaptErr) . Service.runFx env
          where
            adaptErr = Service.renderFxErrForPublicAsPlainText

-- |
-- Start a server, which logs the output to the provided handle.
--
-- Completely encapsulates the whole server implementation.
startServer ::
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
startServer port loggerHandle loggerVerbosity db timer =
  startService loggerHandle loggerVerbosity db timer
    >>= Api.serve port
