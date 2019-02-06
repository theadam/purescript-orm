module Connection where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Effect.Exception (Error)
import Foreign (Foreign)
import Effect.Aff (Aff)
import Data.Tuple (Tuple)
import ParameterizedSql (Value)

data Operation
  = Insert String (Array String) (Array (Array Value))
  | Create Boolean String (Array (Tuple String String))
  | Truncate String

class (Monad m, MonadError Error m) <= MonadQuerier m where
  runOperation :: Operation -> m (Array (Array Foreign))
  runCommand :: Operation -> m Int
  withTransaction :: forall a. m a -> m a

class (MonadQuerier m) <= MonadConnection conn m | m -> conn, conn -> m where
  withConnection :: forall a. conn -> m a -> Aff a

