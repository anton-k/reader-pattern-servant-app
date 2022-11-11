-- | Reader pattern monad
module App
  ( App
  , ApiError
  , runApp
  , module X
  ) where

import Data.Text (Text)
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X
import Control.Monad.Except as X

type ApiError = Text

newtype App env a = App (ReaderT env (ExceptT ApiError IO) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader env
    , MonadError ApiError
    , MonadIO
    )

runApp :: App env a -> env -> IO (Either ApiError a)
runApp (App a) env = runExceptT $ runReaderT a env
