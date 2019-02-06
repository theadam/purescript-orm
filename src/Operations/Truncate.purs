module Operations.Truncate where

import Prelude

import Connection (class MonadQuerier, runOperation, Operation(..))
import TableDefinition (Table)

truncate :: forall m cs. MonadQuerier m => Table cs -> m Unit
truncate t = (runOperation $ Truncate t.name) $> unit
