-- | Time-service interface
module DI.Time
  ( Time(..)
  , HasTime
  , askTime
  ) where

import Types

data Time = Time
  { now :: IO UTCTime
  }

type HasTime env = HasField "time" env Time

askTime :: HasTime env => App env Time
askTime = asks (.time)
