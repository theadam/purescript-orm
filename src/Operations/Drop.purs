module Operations.Drop where

import Prelude

import Connection (class MonadQuerier, runOperation, Operation(..))
import TableDefinition (Table)

drop :: forall m cs. MonadQuerier m => Table cs -> m Unit
drop t = (runOperation $ Drop t.name) $> unit
