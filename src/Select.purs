module Select where

import Prelude

import Column (And, Column)
import Data.Array (singleton, uncons)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Database.PostgreSQL (fromSQLValue)
import Record (LProxy(..))
import Record as R
import SQLExpression (class SQLExpression, toTypedSQLExpression)
import Type.Data.Boolean (False, True, kind Boolean)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, kind RowList)
import Utils (ParamSQL, Selectable(Selectable))

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

class SelectMappable a b (isRec :: Boolean) | a -> b, a b -> isRec where
  mapToResult :: a -> Array Foreign -> Either String (Tuple (Array Foreign) b)
  getSelects :: a -> Array ParamSQL

instance nestSelectMappable
  :: ( SelectMapping s r
     , RowToList s sl
     , HasSelectList sl s
     ) => SelectMappable (Record s) (Record r) True where
       mapToResult s res = mapSelect s res
       getSelects s = selectsFromRowList (LProxy :: LProxy sl) s

instance selectableMappable
  :: ( SQLExpression i o
     ) => SelectMappable i o False where
       mapToResult sel res = case uncons res of
         Nothing -> Left "Invalid result set"
         Just { head, tail } -> Tuple tail <$> fromSQLValue head
       getSelects = singleton <<< toTypedSQLExpression

class HasSelectList (rl :: RowList) (r :: # Type) | rl -> r where
  selectsFromRowList :: LProxy rl -> Record r -> Array ParamSQL

instance nilHasSelectList :: HasSelectList Nil () where
  selectsFromRowList _ _ = []

instance consHasSelectList
  :: ( IsSymbol k
     , SelectMappable a b bb
     , RowToList r (Cons k a rl')
     , RowCons k a r' r
     , HasSelectList rl' r'
     ) => HasSelectList (Cons k a rl') r where
     selectsFromRowList _ r = getSelects sel <> rest
       where
         keyProxy = SProxy :: SProxy k
         sel = R.get keyProxy r
         rest = selectsFromRowList (LProxy :: LProxy rl') (R.delete keyProxy r)

class SelectMapping (s :: # Type) (r :: # Type) | s -> r where
  mapSelect :: Record s -> Array Foreign -> Either String (Tuple (Array Foreign) (Record r))

instance selectMappingImpl
  :: ( RowToList s sl
     , RowToList r rl
     , SelectListMapping sl rl s r
     ) => SelectMapping s r where
  mapSelect = mapSelect' (LProxy :: LProxy sl)

class SelectListMapping (sl :: RowList) (rl :: RowList) s r | sl -> rl , sl -> s , sl -> r where
  mapSelect' :: LProxy sl -> Record s -> Array Foreign -> Either String (Tuple (Array Foreign) (Record r))

instance nilSelectableRowList :: SelectListMapping Nil Nil () () where
  mapSelect' _ s res = pure $ Tuple res {}

instance consSelectableRowList
  :: ( IsSymbol k
     , SelectMappable i o bb
     , RowCons k i s' s
     , RowCons k o r' r
     , SelectListMapping sl' rl' s' r' )
     => SelectListMapping (Cons k i sl') (Cons k o rl') s r where
       mapSelect' _ s res = Tuple <$>
         (fst <$> rest) <*>
         (R.insert keyProxy <$> (snd <$> val) <*> (snd <$> rest))
           where
             keyProxy = SProxy :: SProxy k
             s' = R.delete keyProxy s
             sel = R.get keyProxy s
             rest = (fst <$> val) >>= mapSelect' (LProxy :: LProxy sl') s'
             val = mapToResult sel res
