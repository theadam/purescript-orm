module Connection where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Expression (BoolExp)
import Foreign (Foreign)
import ParameterizedSql (ParameterizedSql, Value)
import TableDefinition (ColumnDefinition)

-- From table alias
data From = From String String
data Join
  = Inner From BoolExp
  | Left From BoolExp
  | Outer From BoolExp
  | Right From BoolExp

type SelectData =
  { froms :: Array From
  , wheres :: Array BoolExp
  , joins :: Array Join
  }

baseSelectData :: SelectData
baseSelectData =
  { froms: []
  , wheres: []
  , joins: []
  }

type ReturnInsertResults = Boolean

data Operation
  = Insert String (Array String) (Array (Array Value)) ReturnInsertResults
  | Create Boolean String (Array (Tuple String ColumnDefinition))
  | Truncate String
  | Drop String
  | Select (Array ParameterizedSql) SelectData

class (Monad m, MonadError Error m) <= MonadConverter m where
  convertColumnDefinition :: ColumnDefinition -> m String
  convertOperation :: Operation -> m ParameterizedSql

class (MonadConverter m) <= MonadQuerier m where
  runOperation :: Operation -> m (Array (Array Foreign))
  runCommand :: Operation -> m Int
  withTransaction :: forall a. m a -> m a

class (MonadQuerier m) <= MonadConnection conn m | m -> conn, conn -> m where
  withConnection :: forall a. conn -> m a -> Aff a

