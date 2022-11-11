-- | Data model.
-- Request and response structures.
module ReaderProtoApi.Model where

import Deriving.Aeson
import Deriving.Aeson.Stock
import ReaderProtoBase.Prelude
import Servant.API qualified as Servant

data SaveRequest = SaveRequest
  { message :: Text,
    tags :: [Tag]
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Vanilla SaveRequest

newtype MessageId = MessageId {value :: Int}
  deriving newtype (ToJSON, FromJSON, Show, Eq, Ord, Servant.FromHttpApiData)

newtype Tag = Tag {value :: Text}
  deriving newtype (ToJSON, FromJSON, Show, Eq, Servant.FromHttpApiData)

data Message = Message
  { content :: Text,
    tags :: [Tag],
    time :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Vanilla Message
