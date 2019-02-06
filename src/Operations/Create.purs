module Operations.Create where

import Prelude

import Classes.Create (class Creatable, columnDefinitions)
import Connection (class MonadQuerier, runOperation, Operation(..))
import TableDefinition (Table)

createBase :: forall m cs
  . MonadQuerier m
  => Creatable cs
  => Boolean -> Table cs -> m Unit
createBase mod t = do
  _ <- runOperation $ Create mod t.name (columnDefinitions t.columns)
  pure unit


create :: forall m cs
  . MonadQuerier m
  => Creatable cs
  => Table cs -> m Unit
create = createBase false

createIfNotExists :: forall m cs
  . MonadQuerier m
  => Creatable cs
  => Table cs -> m Unit
createIfNotExists = createBase true

