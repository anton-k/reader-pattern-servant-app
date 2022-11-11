-- |
-- Servant implementation of the API with actual behaviour to be provided
-- via dependency injection.
--
-- This lib is solely focused on routing and mapping. It has 0 domain
-- knowledge, which is delegated to the user via the 'Service' type.
-- In its turn the user of this lib needs to know nothing about servant to
-- be able to spin up a server. All that he should care of is implementing
-- the 'Service' type.
--
-- Ideally this lib should be generated from OpenAPI/Swagger spec.
module ReaderProtoApi
  ( Service (..),
    Api,
    server,
    ReaderProtoApi.serve,

    -- * Model
    module ReaderProtoApi.Model,
  )
where

import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Text.Encoding qualified as TextEncoding
import Network.Wai.Handler.Warp qualified as Warp
import ReaderProtoApi.Model
import ReaderProtoBase.Prelude hiding (Handler)
import Servant

-- * Servant API

type Api =
  "api" :> "v1" :> V1Api

type V1Api =
  SaveMessage :<|> GetById :<|> GetByTag :<|> ToggleLog

type SaveMessage =
  "save" :> ReqBody '[JSON] SaveRequest :> Post '[JSON] MessageId

type GetById =
  "get" :> "message" :> Capture "id" MessageId :> Get '[JSON] Message

type GetByTag =
  "get" :> "tag" :> Capture "tag" Tag :> Get '[JSON] [Message]

type ToggleLog =
  "toggle-logs" :> Post '[JSON] Bool

-- * Injectable Env

-- | Injectable behaviour of the server.
data Service = Service
  { save :: SaveRequest -> IO (Either Text MessageId),
    getById :: MessageId -> IO (Either Text Message),
    getByTag :: Tag -> IO (Either Text [Message]),
    toggleLog :: IO (Either Text Bool)
  }

-- * Servant Server

server :: Service -> Server Api
server service =
  eitherIOHandler service.save
    :<|> eitherIOHandler service.getById
    :<|> eitherIOHandler service.getByTag
    :<|> eitherIOHandler (const service.toggleLog) ()
  where
    eitherIOHandler :: (req -> IO (Either Text res)) -> req -> Handler res
    eitherIOHandler handler req =
      liftIO (handler req) >>= \case
        Right res -> return res
        Left err ->
          throwError $
            err400
              { errBody =
                  ByteStringLazy.fromStrict $
                    TextEncoding.encodeUtf8 err
              }

-- * Execution

serve :: Int -> Service -> IO ()
serve port service =
  Warp.run port $
    Servant.serve (Proxy @Api) (server service)
