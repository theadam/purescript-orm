module Classes.Select where

import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList (kind RowList, Nil, Cons, class RowToList)
import Record (insert)
import TableDefinition (class ColumnType)
import Type.Data.Row (RProxy)
import Type.Equality (class TypeEquals, to)
import Type.Prelude (RLProxy(..), SProxy(..))
import Utils (ColumnAlias(..))

class Selectable (t :: # Type) exprs | t -> exprs where
  toSelectColumns :: RProxy t -> String -> exprs

instance selectableTable ::
  (RowToList t rl, SelectableColumns rl exprs) => Selectable t exprs where
  toSelectColumns _ alias = generateSelectableColumns (RLProxy :: RLProxy rl) alias

class SelectableColumns (rl :: RowList) exprs | rl -> exprs where
  generateSelectableColumns :: RLProxy rl -> String -> exprs

instance nilSelectableColumns :: (TypeEquals (Record ()) r) => SelectableColumns Nil r where
    generateSelectableColumns _ _ = to {}
else instance consSelectableColumns ::
 (
   IsSymbol k,
   Row.Lacks k r',
   Row.Cons k (ColumnAlias k o) r' r,
   SelectableColumns rl' (Record r'),
   ColumnType v i o
 ) => SelectableColumns (Cons k v rl') (Record r) where
   generateSelectableColumns _ alias = insert (SProxy :: SProxy k) (ColumnAlias alias) (generateSelectableColumns (RLProxy :: RLProxy rl') alias)
