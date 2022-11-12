-- | Get by tag handler
module Server.GetByTag
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
  { getByTag :: Tag -> IO [Message]
  }

-----------------------------------------
-- Handler

handle :: Tag -> App Env [Message]
handle tag = do
  Db{..}  <- asks (.db)
  Log{..} <- askLog

  liftIO $ do
    logInfo $ "get by tag call: " <> display tag
    getByTag tag
