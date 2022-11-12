-- | Reader pattern monad
module App
  ( App
  , ApiError (..)
  , throwApi
  , runApp
  , module X
  ) where

import Data.Text (Text)
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X
import Control.Monad.Except as X
import Control.Monad.Catch as X (MonadThrow(..), MonadCatch(..))
import Servant.Server
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
    )

runApp :: App env a -> env -> IO a
runApp (App a) env = runReaderT a env

newtype ApiError = ApiError Text

throwApi :: ApiError -> App env a
throwApi = throwM . toServantError
  where
    toServantError (ApiError err) = err400 { errBody = BL.fromStrict $ Text.encodeUtf8 err }


