module Server.Class
  ( HasDb
  , HasTime
  , HasLog
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
import GHC.Records (HasField(..))

-- type classes to specify which interface environment has

type HasDb   env = HasField "db" env IDb
type HasTime env = HasField "time" env ITime
type HasLog  env = HasField "log" env ILogVar

-- helpers

askDb :: HasDb env => App env IDb
askDb = asks (.db)

askTime :: HasTime env => App env ITime
askTime = asks (.time)

askLog :: HasLog env => App env ILog
askLog = liftIO . readLogVar =<< asks (.log)

