module ReaderProtoMockDb
  ( MockDb,
    start,
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

insert :: MockDb -> Message -> IO Int
insert db msg = atomically $ do
  msgId <- Map.size <$> readTVar db.map
  modifyTVar' db.map (Map.insert msgId msg)
  pure msgId

lookup :: MockDb -> Int -> IO (Maybe Message)
lookup db msgId = Map.lookup msgId <$> readTVarIO db.map

lookupByTag :: MockDb -> Text -> IO [Message]
lookupByTag db tag =
  filter (\msg -> elem tag msg.tags) . Map.elems <$> readTVarIO db.map
