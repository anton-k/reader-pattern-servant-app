-- | Setup and update configs and initialization params
module DI.Setup
  ( Setup(..)
  , HasSetup
  , askSetup
  ) where

import Types

-- | Setup app configs
data Setup = Setup
  { toggleLogs :: IO ()  -- ^ toggle log verbosity
  }

type HasSetup env = HasField "setup" env Setup

askSetup :: HasSetup env => App env Setup
askSetup = asks (.setup)
