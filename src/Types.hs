module Types
  ( Url
  , App
  , runApp
  , MessageId(..)
  , Tag(..)
  , Message(..)
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Time (UTCTime)
import Data.Text (Text)
import Deriving.Aeson
import Deriving.Aeson.Stock
import Servant.API (FromHttpApiData)

type Url = String
type ApiError = Text

type App env a = ReaderT env (ExceptT ApiError IO) a

runApp :: App env a -> env -> IO (Either ApiError a)
runApp a env = runExceptT $ runReaderT a env

newtype MessageId = MessageId { unMessageId :: Int }
  deriving newtype (ToJSON, FromJSON, Show, Eq, Ord, FromHttpApiData)

newtype Tag = Tag { unTag :: Text }
  deriving newtype (ToJSON, FromJSON, Show, Eq, FromHttpApiData)

data Message = Message
  { content :: Text
  , tags    :: [Tag]
  , time    :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Vanilla Message

