module Operations.Select where

import Prelude

import Classes.Select (class Fromable, class SelectResult, createSelects, mapResultRow, toFroms)
import Connection (class MonadQuerier, From(..), Operation(..), SelectData, baseSelectData, runOperation)
import Control.Monad.State (State, get, modify_, runState)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Expression (BoolExp)
import TableDefinition (Table)
import Type.Prelude (Proxy(..))
import Utils (convertEither)

type SelectState =
  { aliasCount :: Int
  , select :: SelectData
  }

baseSelectState :: SelectState
baseSelectState =
  { aliasCount: 0
  , select: baseSelectData
  }

type Select r = State SelectState r

from :: forall cs r. Fromable cs r => Table cs -> Select r
from t = do
  { aliasCount } <- get
  let count = aliasCount + 1

  let alias = t.name <> show count

  modify_ \s -> s { aliasCount = count, select { froms = s.select.froms <> [From t.name alias]} }
  pure $ toFroms t.columns alias

select :: forall m r o. MonadQuerier m => SelectResult r o => Select r -> m (Array o)
select s = do
  aaf <- runOperation op

  let outputs = sequence $ mapResultRow (Proxy :: Proxy r) <$> aaf
  map snd <$> convertEither outputs
    where
      (Tuple res state) = runState s baseSelectState
      op = Select (createSelects res) state.select

-- Kinda like where, but called filter due to reserved words
filter :: BoolExp -> Select Unit
filter w = modify_ \s -> s { select { wheres = s.select.wheres <> [w] } }
