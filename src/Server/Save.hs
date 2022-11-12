-- | Save message handler
module Server.Save
  ( Env(..)
  , Db(..)
  , handle
  ) where

import DI.Log
import DI.Time

import Api
import Types

data Db = Db
  { saveMessage :: Message -> IO MessageId
  }

data Env = Env
  { log   :: Log
  , db    :: Db
  , time  :: Time
  }

-----------------------------------------
-- Handler

handle :: SaveRequest -> App Env SaveResponse
handle req = do
  Time{..} <- askTime
  Db{..}   <- asks (.db)
  Log{..}  <- askLog

  liftIO $ do
    logInfo $ "save call: " <> display req
    time <- now
    let msg = Message req.message req.tags time
    logInfo $ "create message: " <> display msg
    SaveResponse <$> saveMessage msg
