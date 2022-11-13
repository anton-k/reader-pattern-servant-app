-- | Reader pattern monad
module App
  ( App
  , ApiError (..)
  , throwApi
  , runApp
  , toHandler
  , toHandler1
  , module X
  ) where

import Data.Text (Text)
import Control.Exception (try)
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X
import Control.Monad.Except as X
import Control.Monad.Catch as X (MonadThrow(..), MonadCatch(..))
import Servant.Server as Servant
import Data.ByteString.Lazy   qualified as BL
import Data.Text.Encoding     qualified as Text

newtype App env a = App (ReaderT env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader env
    , MonadIO
    , MonadThrow
    , MonadCatch
    )

runApp :: App env a -> env -> IO a
runApp (App a) env = runReaderT a env

toHandler :: env -> App env resp -> Servant.Handler resp
toHandler e handler = Handler $ ExceptT $ try $ runApp handler e

toHandler1 :: env -> (req -> App env resp) -> req -> Servant.Handler resp
toHandler1 env handle a = toHandler env (handle a)

newtype ApiError = ApiError Text

throwApi :: ApiError -> App env a
throwApi = throwM . toServantError
  where
    toServantError (ApiError err) = err400 { errBody = BL.fromStrict $ Text.encodeUtf8 err }

