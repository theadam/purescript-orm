module Operations.Truncate where

import Prelude

import Connection (class MonadConnection, runOperation, Operation(..))
import TableDefinition (Table)

truncate :: forall m conn cs. MonadConnection conn m => Table cs -> m Unit
truncate t = (runOperation $ Truncate t.name) $> unit
