module Server.GetById
  ( GetByIdEnv(..)
  , handleGetById
  ) where

import Server.Env
import Server.Class
import Types

-----------------------------------------
-- Env

data GetByIdEnv = GetByIdEnv
  { db  :: IDb
  , log :: ILogVar
  }

-----------------------------------------
-- Handler

handleGetById :: MessageId -> App GetByIdEnv Message
handleGetById messageId = do
  IDb{..}  <- askDb
  ILog{..} <- askLog

  mMsg <- liftIO $ do
    logInfo $ "get by id call: " <> display messageId
    getMessage messageId
  case mMsg of
    Just msg -> pure msg
    Nothing  -> do
      liftIO $ logError $ "Message not found by id: " <> display messageId
      throwError "Message not found"
