-- | init time service
module DI.Time
  ( initTime
  ) where

import Data.Time
import Server.Env
import Types

initTime :: Url -> IO ITime
initTime _url = do
  pure $ ITime
    { now = getCurrentTime
    }
