-- | Time-service interface, it's here as an example of
-- external service that talks to the outside world.
-- Let's imagine that we ask current time over http-client
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
