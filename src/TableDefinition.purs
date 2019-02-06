module TableDefinition where

import Prelude

import Type.Data.Row (RProxy(..))
import Utils (class Keys, keys)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import ParameterizedSql (Value(..), class ToValue)
import Type.Proxy (Proxy(..))

type Table (cs :: # Type) = {
  columns :: RProxy cs,
  columnNames :: Array String,
  name :: String
}

makeTable ::
  forall cs.
    Keys (RProxy cs)
    => String -> Table cs
makeTable name = {
  columns: colProxy,
  columnNames: columnNames,
  name: name
}
  where
    colProxy :: RProxy cs
    colProxy = RProxy
    columnNames = keys colProxy

data ColumnDefinition
  = Id
  | Varchar Int
  | NotNull ColumnDefinition
  | Nullable ColumnDefinition
  | Default String ColumnDefinition


class ColumnType cd i o | cd -> i, cd -> o where
  columnDefinition :: Proxy cd -> ColumnDefinition

data Id

instance idColumnType :: ColumnType Id (Maybe Id) Int where
  columnDefinition _ = Id

instance valueId :: ToValue Id where
  toValue _ = NullValue

class Lengthable a where
  fieldLength :: Proxy a -> Int

data Normal
instance normalLength :: Lengthable Normal where
  fieldLength _ = 255

data Varchar l
instance varcharType :: Lengthable l => ColumnType (Varchar l) String String where
  columnDefinition _ = Varchar $ fieldLength l
    where
      l = Proxy :: Proxy l

type StringColumn = Varchar Normal

data NotNull cd
instance notNullType :: (ColumnType cd i o) => ColumnType (NotNull cd) i o where
  columnDefinition _ = NotNull $ columnDefinition (Proxy :: Proxy cd)

data Nullable cd
instance nullType :: (ColumnType cd i o) => ColumnType (Nullable cd) (Maybe i) (Maybe o) where
  columnDefinition _ = Nullable $ columnDefinition (Proxy :: Proxy cd)

data Default (def :: Symbol) cd
instance defaultType :: (ColumnType cd i o, IsSymbol a) => ColumnType (Default a cd) (Maybe i) o where
    columnDefinition _ = Default (reflectSymbol sproxy) (columnDefinition proxy)
      where
        proxy = Proxy :: Proxy cd
        sproxy = SProxy :: SProxy a
