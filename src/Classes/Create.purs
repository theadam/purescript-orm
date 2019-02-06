module Classes.Create where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.RowList (Cons, Nil, kind RowList, class RowToList)
import TableDefinition (class ColumnType, ColumnDefinition, columnDefinition)
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(RLProxy))
import Type.Proxy (Proxy(..))

class Creatable (t :: # Type) where
  columnDefinitions :: RProxy t -> Array (Tuple String ColumnDefinition)

instance rowCreatable :: (RowToList r rl, CreateCols rl) => Creatable r where
  columnDefinitions _ = coldefs (RLProxy :: RLProxy rl)

-- Row List Handling
class CreateCols (rl :: RowList) where
  coldefs :: RLProxy rl -> Array (Tuple String ColumnDefinition)

instance singleListCreatable :: (IsSymbol k, ColumnType v i o) => CreateCols (Cons k v Nil) where
  coldefs _ = [Tuple (reflectSymbol (SProxy :: SProxy k)) (columnDefinition (Proxy :: Proxy v))]
else instance listCreatable :: (
  IsSymbol k,
  ColumnType v i o,
  CreateCols rest
) => CreateCols (Cons k v rest) where
  coldefs _ = [Tuple (reflectSymbol (SProxy :: SProxy k)) (columnDefinition (Proxy :: Proxy v))] <> coldefs (RLProxy :: RLProxy rest)
