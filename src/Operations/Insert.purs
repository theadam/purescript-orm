module Operations.Insert where

import Prelude

import Classes.Insert (class InsertResult, class Insertable, values, mapResult)
import Connection (class MonadConnection, runOperation, Operation(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (Writer, tell, execWriter)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Exception (error)
import ParameterizedSql (Value)
import TableDefinition (Table)

insertBase :: forall m conn res cs
  . MonadConnection conn m
  => InsertResult cs res
  => Table cs -> Array (Array Value) -> m (Array res)
insertBase t r = do
  fs <- runOperation $ Insert t.name t.columnNames r
  sequence $ mapResult t.columns <$> fs

insert :: forall m conn r res cs
  . MonadConnection conn m
  => Insertable cs r
  => InsertResult cs res
  => Table cs -> r -> m res
insert t r = do
  rs <- insertBase t [values t.columns r]
  case head rs of
    Nothing -> throwError $ error "Empty result from insert"
    Just a -> pure a

insertBatch :: forall m cs conn res
  . MonadConnection conn m
  => InsertResult cs res
  => Table cs
  -> ((forall r. Insertable cs r => r -> Writer (Array (Array Value)) Unit) -> Writer (Array (Array Value)) Unit) -> m (Array res)
insertBatch t fn = insertBase t (execWriter $ fn vs)
  where
    vs :: forall r. Insertable cs r => r -> Writer (Array (Array Value)) Unit
    vs r = tell $ [values t.columns r]

