module Insert where

import Prelude

import Column (class ColumnType, And, Column)
import Data.Array (filter, snoc)
import Data.Foldable (foldl)
import Data.Foreign (Foreign, isNull)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap, keys, singleton, union, values)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Database.PostgreSQL (toSQLValue)
import Defaults (class Defaults, withDefaults)
import Exec (AffOrm, Runner)
import Record as R
import Table (Table, columns, tableName)
import Type.Data.Boolean (kind Boolean)
import Type.Proxy (Proxy(..))

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

data Placeholder = Default | Param
data Vals = Single (StrMap Foreign) | Multiple Vals (StrMap Foreign)

instance semiGroupVals :: Semigroup Vals where
  append v (Single w) = Multiple v w
  append v (Multiple ws w) = Multiple (v <> ws) w

newtype Insert t = Insert
 { vals :: Vals
 , placeholders :: Array (Array Placeholder)
 }
derive instance insertNewtype :: Newtype (Insert t) _

instance semiGroupInsert :: Semigroup (Insert t) where
  append (Insert a) (Insert b) = Insert
   { vals: a.vals <> b.vals
   , placeholders: a.placeholders <> b.placeholders
   }

replacePlaceholders :: Array (Array Placeholder) -> Array (Array String)
replacePlaceholders ps = snd $ foldl outer (Tuple 1 []) ps
  where
    outer (Tuple i rows) ary = Tuple (fst res) (snoc rows (snd res))
      where
        res = foldl runner (Tuple i []) ary
    runner (Tuple i row) Default = Tuple i (snoc row "DEFAULT")
    runner (Tuple i row) Param = Tuple (i + 1) (snoc row ("$" <> show i))

valsHead :: Vals -> StrMap Foreign
valsHead (Single s) = s
valsHead (Multiple _ s) = s

valParams :: Vals -> Array Foreign
valParams (Single map) = values map
valParams (Multiple vals map) = valParams vals <> values map

class Insertable t a where
  toInsert :: Proxy t -> a -> Insert t

instance insertableInsertRecord
  :: ( InsertRecord cd r
     , Defaults i r
     ) => Insertable (Table n cd) (Record i) where
  toInsert t a = Insert
    { vals: Single pairs
    , placeholders: [placeholders]
    }
      where
        placeholders = map toPlaceholder (values pairs)
        toPlaceholder v | isNull v = Default
                        | otherwise = Param
        pairs = toPairs (columns t) (withDefaults a)

insertToSql :: forall n cd. IsSymbol n => Insert (Table n cd) -> Tuple String (Array Foreign)
insertToSql (Insert { vals, placeholders }) = Tuple sql params
  where
    sql = space ["INSERT INTO", tableName (Proxy :: Proxy (Table n cd)), cols, "VALUES", ps]
    params = filter (not <<< isNull) $ valParams vals
    ps = comma $ map (surround <<< comma) (replacePlaceholders placeholders)
    vh = valsHead vals
    cols = surround (comma (keys vh))
    comma = joinWith ", "
    space = joinWith " "
    surround str = "(" <> str <> ")"

insertInto :: forall n r cd fx. IsSymbol n => Insertable (Table n cd) r => Runner fx -> Proxy (Table n cd) -> r -> AffOrm fx Unit
insertInto runner table rec = unit <$ ((uncurry runner) $ insertToSql $ toInsert table rec)


