-- | Get message by id handler
module Server.GetMessage
  ( Env(..)
  , Db(..)
  , handle
  ) where

import DI.Log
import Types

-----------------------------------------
-- Env

data Env = Env
  { db  :: Db
  , log :: Log
  }

data Db = Db
  { getMessage :: MessageId -> IO (Maybe Message)
  }

-----------------------------------------
-- Handler

handle :: MessageId -> App Env Message
handle messageId = do
  Db{..}  <- asks (.db)
  Log{..} <- askLog

  mMsg <- liftIO $ do
    logInfo $ "get by id call: " <> display messageId
    getMessage messageId
  case mMsg of
    Just msg -> pure msg
    Nothing  -> do
      liftIO $ logError $ "Message not found by id: " <> display messageId
      throwError "Message not found"
