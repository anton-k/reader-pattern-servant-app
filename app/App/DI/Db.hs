-- | Init db service (using mock one for prototype)
module App.DI.Db
  ( initDb
  ) where

import App.DI.Db.MockDb

import Server (Db(..))
import Server.Save qualified as Save
import Server.GetMessage qualified as GetMessage
import Server.ListTag qualified as ListTag
import Types

initDb :: Url -> IO Db
initDb _url = do
  db <- newMockDb
  pure $ Db
    { save       = Save.Db { saveMessage = insertDb db }
    , getMessage = GetMessage.Db { getMessage = lookupDb db }
    , listTag    = ListTag.Db { listTag = lookupByTag db }
    }
