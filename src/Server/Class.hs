module Server.Class
  ( HasDb(..)
  , HasTime(..)
  , HasLog(..)
  , module X
  , askDb
  , askTime
  , askLog
  ) where

import Control.Monad.IO.Class as X
import Control.Monad.Reader as X
import Control.Monad.Except as X
import Server.Env
import Types

class HasDb env where
  getDb :: env -> IDb

class HasTime env where
  getTime :: env -> ITime

class HasLog env where
  getLog :: env -> ILogVar

-- helpers

askDb :: HasDb env => App env IDb
askDb = asks getDb

askTime :: HasTime env => App env ITime
askTime = asks getTime

askLog :: HasLog env => App env ILog
askLog = liftIO . readLogVar =<< asks getLog

