module Insert where

import Prelude

import Column (class ColumnType, And, Column)
import Data.Array (mapWithIndex)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, keys, singleton, union, values)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (toSQLValue)
import Exec (Runner, AffOrm)
import Record (LProxy(..))
import Record as R
import Table (Table)
import Type.Data.Boolean (kind Boolean)
import Type.Data.Symbol (class Equals)
import Type.Prelude (BProxy(..), False, True)
import Type.Proxy (Proxy(..))
import Type.Row (kind RowList, class RowToList, Cons, Nil)

class InsertRecord cd r | cd -> r where
  toPairs :: Proxy cd -> Record r -> StrMap Foreign

instance colInsertRecord ::
  (ColumnType cd i o, IsSymbol n, RowCons n i () r) =>
  InsertRecord (Column n cd) r where
    toPairs _ r = singleton (reflectSymbol sproxy) (toSQLValue (R.get sproxy r))
      where
        proxy = Proxy :: Proxy cd
        sproxy = SProxy :: SProxy n

instance colInsertAnd ::
  (InsertRecord c1 r1, InsertRecord c2 r2, Union r2 r1 r3, Union r1 r2 r3) =>
  InsertRecord (And c1 c2) r3 where
    toPairs _ r =
      union
        (toPairs (Proxy :: Proxy c1) (R.subrecord r))
        (toPairs (Proxy :: Proxy c2) (R.subrecord r))

mapValues :: forall a. Show a => Array a -> Array String
mapValues = mapWithIndex (\i _ -> "$" <> show (i + 1))

insertSql :: forall cd r n. InsertRecord cd r => IsSymbol n => Proxy (Table n cd) -> Record r -> Tuple String (Array Foreign)
insertSql _ r = Tuple sql vs
  where
      pairs = toPairs (Proxy :: Proxy cd) r
      vs = values pairs
      placeholders = joinWith "," (mapValues $ keys pairs)
      ks = joinWith ", " (keys pairs)
      sql = "INSERT INTO " <>
            reflectSymbol (SProxy :: SProxy n) <>
            " (" <> ks <> ")" <>
            " VALUES " <>
            " (" <> placeholders <> ")"



insert :: forall fx cd r n. InsertRecord cd r => IsSymbol n => Runner fx -> Proxy (Table n cd) -> Record r -> AffOrm fx Unit
insert runner t r = unit <$ do
  let (Tuple sql params) = insertSql t r
  runner sql params


class DefaultMaybes (ol :: RowList) (o :: # Type) | ol -> o where
  allNothings :: LProxy ol -> Record o
instance nilMaybes :: DefaultMaybes Nil () where
  allNothings _ = {}
instance consMaybes
  :: ( IsSymbol k
     , DefaultMaybes ol' o'
     , RowCons k (Maybe t) o' o
     ) => DefaultMaybes (Cons k (Maybe t) ol') o where
       allNothings _ = R.insert keyProxy Nothing (allNothings (LProxy :: LProxy ol'))
         where
           keyProxy = SProxy :: SProxy k

class RecMaybe (il :: RowList) (ol :: RowList) (i :: # Type) (o :: # Type)
  | ol -> o, il -> i where
    withDefaults' :: LProxy il -> LProxy ol -> Record i -> Record o

instance nilRecMaybe :: DefaultMaybes ol o => RecMaybe Nil ol () o where
  withDefaults' _ _ _ = allNothings (LProxy :: LProxy ol)

instance consMaybeRecMaybe
  :: ( IsSymbol k
     , Equals j k b
     , RecMaybeHelper (Cons j s il') (Cons k t ol') i o b
     ) => RecMaybe (Cons j s il') (Cons k t ol') i o where
  withDefaults' il ol r = withDefaultsHelper (BProxy :: BProxy b) il ol r

instance failRecMaybe
  :: ( IsSymbol k
     , Fail (TypeConcat (TypeConcat "'" k) "' is an invalid key for this record")
     ) => RecMaybe (Cons k t il') Nil i () where
       withDefaults' _ _ _ = {}

class RecMaybeHelper (il :: RowList) (ol :: RowList) (i :: # Type) (o :: # Type) (b :: Boolean) where
  withDefaultsHelper :: BProxy b -> LProxy il -> LProxy ol -> Record i -> Record o

instance hasRecMaybeHelper
  :: ( IsSymbol k
     , RecMaybe il' ol' i' o'
     , RowCons k t i' i
     , RowCons k t o' o
     ) => RecMaybeHelper (Cons k t il') (Cons k t ol') i o True where
  withDefaultsHelper _ _ _ r = R.insert keyProxy (R.get keyProxy r) rest
    where
      keyProxy = SProxy :: SProxy k
      rest = withDefaults' (LProxy :: LProxy il') (LProxy :: LProxy ol') (R.delete keyProxy r)

instance hasMaybeRecMaybeHelper
  :: ( IsSymbol k
     , RecMaybe il' ol' i' o'
     , RowCons k t i' i
     , RowCons k (Maybe t) o' o
     ) => RecMaybeHelper (Cons k t il') (Cons k (Maybe t) ol') i o True where
  withDefaultsHelper _ _ _ r = R.insert keyProxy (Just $ R.get keyProxy r) rest
    where
      keyProxy = SProxy :: SProxy k
      rest = withDefaults' (LProxy :: LProxy il') (LProxy :: LProxy ol') (R.delete keyProxy r)

instance doesntHaveRecMaybeHelper
  :: ( IsSymbol k
     , RecMaybe il ol' i o'
     , RowCons k (Maybe t) o' o
     ) => RecMaybeHelper il (Cons k (Maybe t) ol') i o False where
  withDefaultsHelper _ _ _ r = R.insert keyProxy Nothing rest
    where
      keyProxy = SProxy :: SProxy k
      rest = withDefaults' (LProxy :: LProxy il) (LProxy :: LProxy ol') r

class Defaults (i :: # Type) (o :: # Type) where
  withDefaults :: Record i -> Record o

instance defaultsImpl :: (RowToList i il, RowToList o ol, RecMaybe il ol i o) => Defaults i o where
  withDefaults i = withDefaults' (LProxy :: LProxy il) (LProxy :: LProxy ol) i





