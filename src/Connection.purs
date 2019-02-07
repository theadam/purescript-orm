module Connection where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Foreign (Foreign)
import ParameterizedSql (ParameterizedSql, Value)
import TableDefinition (ColumnDefinition)

-- From table alias
data From = From String String

data Where
  = Cond ParameterizedSql
  | And Where Where
  | Or Where Where

type SelectData =
  { froms :: Array From
  , wheres :: Array Where
  }

baseSelectData :: SelectData
baseSelectData =
  { froms: []
  , wheres: []
  }

data Operation
  = Insert String (Array String) (Array (Array Value))
  | Create Boolean String (Array (Tuple String ColumnDefinition))
  | Truncate String
  | Drop String
  | Select (Array ParameterizedSql) SelectData

class (Monad m, MonadError Error m) <= MonadQuerier m where
  runOperation :: Operation -> m (Array (Array Foreign))
  runCommand :: Operation -> m Int
  withTransaction :: forall a. m a -> m a

class (MonadQuerier m) <= MonadConnection conn m | m -> conn, conn -> m where
  withConnection :: forall a. conn -> m a -> Aff a

