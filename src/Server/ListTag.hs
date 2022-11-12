-- | Get by tag handler
module Server.ListTag
  ( Env(..)
  , Db(..)
  , handle
  ) where

import DI.Log
import Types

data Env = Env
  { db  :: Db
  , log :: Log
  }

data Db = Db
  { listTag :: Tag -> IO [Message]
  }

-----------------------------------------
-- Handler

handle :: Tag -> App Env [Message]
handle tag = do
  Db{..}  <- asks (.db)
  Log{..} <- askLog

  liftIO $ do
    logInfo $ "list tag call: " <> display tag
    listTag tag
