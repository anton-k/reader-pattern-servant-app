module ReaderProtoMockDb
  ( MockDb,
    start,

    -- * Effects
    runFx,
    Fx,
    insert,
    lookup,
    lookupByTag,

    -- * Data Model
    Message (..),
  )
where

import Data.Map.Strict qualified as Map
import ReaderProtoBase.Prelude

newtype MockDb = MockDb {map :: TVar (Map Int Message)}

data Message = Message
  { content :: Text,
    tags :: [Text],
    time :: UTCTime
  }

start :: IO MockDb
start = MockDb <$> newTVarIO mempty

-- * Effects

runFx :: MockDb -> Fx a -> IO a
runFx db fx = runReaderT fx db

type Fx = ReaderT MockDb IO

insert :: Message -> Fx Int
insert msg = ReaderT $ \db ->
  atomically $ do
    msgId <- Map.size <$> readTVar db.map
    modifyTVar' db.map (Map.insert msgId msg)
    pure msgId

lookup :: Int -> Fx (Maybe Message)
lookup msgId = ReaderT $ \db ->
  Map.lookup msgId <$> readTVarIO db.map

lookupByTag :: Text -> Fx [Message]
lookupByTag tag = ReaderT $ \db ->
  filter (\msg -> elem tag msg.tags) . Map.elems <$> readTVarIO db.map
