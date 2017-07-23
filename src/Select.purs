module Select where

import Prelude

import Column (class ColumnType, And, Column)
import Data.Array (snoc)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Maybe (fromMaybe)
import Data.StrMap (StrMap, lookup)
import Data.Symbol (class IsSymbol, SProxy(..))
import Database.PostgreSQL (fromSQLValue, null)
import Record (LProxy(..))
import Record as R
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, kind RowList)
import Utils (Selectable(..), toSelect)

class SelectRecord cd (r :: # Type) | cd -> r where
  toSelectables :: Proxy cd -> String -> Record r

instance colSelectRecord ::
  (IsSymbol n, RowCons n (Selectable n cd) () r) =>
  SelectRecord (Column n cd) r where
    toSelectables _ alias = R.insert (SProxy :: SProxy n) (Selectable alias) {}

instance andSelectRecord ::
  (SelectRecord s1 r1, SelectRecord s2 r2, Union r1 r2 r) =>
  SelectRecord (And s1 s2) r where
    toSelectables _ alias =
      R.merge
        (toSelectables (Proxy :: Proxy s1) alias)
        (toSelectables (Proxy :: Proxy s2) alias)

class SelectMappable a b | a -> b where
  mapToResult :: a -> StrMap Foreign -> Either String b
  getSelects :: a -> Array String

instance selectableMappable
  :: ( IsSymbol col
     , ColumnType t i o
     ) => SelectMappable (Selectable col t) o where
       mapToResult sel map =
         fromSQLValue (fromMaybe null (lookup (toSelect sel) map))
       getSelects sel = [toSelect sel]

instance nestSelectMappable
  :: ( SelectMapping s r
     , RowToList s sl
     , HasSelectList sl s
     ) => SelectMappable (Record s) (Record r) where
       mapToResult s map = mapSelect s map
       getSelects s = selectsFromRowList (LProxy :: LProxy sl) s

class HasSelectList (rl :: RowList) (r :: # Type) | rl -> r where
  selectsFromRowList :: LProxy rl -> Record r -> Array String

instance nilHasSelectList :: HasSelectList Nil () where
  selectsFromRowList _ _ = []

instance consHasSelectList
  :: ( IsSymbol k
     , IsSymbol col
     , RowToList r (Cons k (Selectable col t) rl')
     , RowCons k (Selectable col t) r' r
     , HasSelectList rl' r'
     ) => HasSelectList (Cons k (Selectable col t) rl') r where
     selectsFromRowList _ r = snoc rest (toSelect sel)
       where
         keyProxy = SProxy :: SProxy k
         sel = R.get keyProxy r
         rest = selectsFromRowList (LProxy :: LProxy rl') (R.delete keyProxy r)

class SelectMapping (s :: # Type) (r :: # Type) | s -> r where
  mapSelect :: Record s -> StrMap Foreign -> Either String (Record r)

instance selectMappingImpl
  :: ( RowToList s sl
     , RowToList r rl
     , SelectListMapping sl rl s r
     ) => SelectMapping s r where
  mapSelect = mapSelect' (LProxy :: LProxy sl)

class SelectListMapping (sl :: RowList) (rl :: RowList) s r | sl -> rl , sl -> s , sl -> r where
  mapSelect' :: LProxy sl -> Record s -> StrMap Foreign -> Either String (Record r)

instance nilSelectableRowList :: SelectListMapping Nil Nil () () where
  mapSelect' _ s map = pure {}

instance consSelectableRowList
  :: ( IsSymbol k
     , SelectMappable i o
     , RowCons k i s' s
     , RowCons k o r' r
     , SelectListMapping sl' rl' s' r' )
     => SelectListMapping (Cons k i sl') (Cons k o rl') s r where
       mapSelect' _ s map = R.insert keyProxy <$> val <*> rest
         where
           keyProxy = SProxy :: SProxy k
           s' = R.delete keyProxy s
           sel = R.get keyProxy s
           rest = mapSelect' (LProxy :: LProxy sl') s' map
           val = mapToResult sel map
