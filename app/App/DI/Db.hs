-- | Init db service (using mock one for prototype)
module App.DI.Db
  ( initDb
  ) where

import App.DI.Db.MockDb

import Server (Db(..))
import Server.Save qualified as Save
import Server.GetById qualified as GetById
import Server.GetByTag qualified as GetByTag
import Types

initDb :: Url -> IO Db
initDb _url = do
  db <- newMockDb
  pure $ Db
    { save = Save.Db { saveMessage = insertDb db }
    , getById = GetById.Db { getMessage = lookupDb db }
    , getByTag = GetByTag.Db { getByTag = lookupByTag db }
    }
