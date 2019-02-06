module Operations.Create where

import Prelude

import Classes.Create (class Creatable, columnDefinitions)
import Connection (class MonadConnection, runOperation, Operation(..))
import TableDefinition (Table)

createBase :: forall m conn cs
  . MonadConnection conn m
  => Creatable cs
  => Boolean -> Table cs -> m Unit
createBase mod t = do
  _ <- runOperation $ Create mod t.name (columnDefinitions t.columns)
  pure unit


create :: forall m conn cs
  . MonadConnection conn m
  => Creatable cs
  => Table cs -> m Unit
create = createBase false

createIfNotExists :: forall m conn cs
  . MonadConnection conn m
  => Creatable cs
  => Table cs -> m Unit
createIfNotExists = createBase true

