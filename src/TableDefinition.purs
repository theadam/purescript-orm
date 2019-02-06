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

class ColumnType cd i o | cd -> i, cd -> o where
  createType :: Proxy cd -> String

data Id

instance idColumnType :: ColumnType Id (Maybe Id) Int where
  createType _ = "SERIAL PRIMARY KEY"

instance valueId :: ToValue Id where
  toValue _ = NullValue

class Lengthable a where
  fieldLength :: Proxy a -> Int

data Normal
instance normalLength :: Lengthable Normal where
  fieldLength _ = 255

data VarChar l
instance varCharType :: Lengthable l => ColumnType (VarChar l) String String where
  createType _ = "VARCHAR(" <> show (fieldLength l) <> ")"
    where
      l = Proxy :: Proxy l

type StringColumn = VarChar Normal

data NotNull cd
instance notNullType :: (ColumnType cd i o) => ColumnType (NotNull cd) i o where
  createType _ = createType (Proxy :: Proxy cd) <> " NOT NULL"

data Nullable cd
instance nullType :: (ColumnType cd i o) => ColumnType (Nullable cd) (Maybe i) (Maybe o) where
  createType _ = createType (Proxy :: Proxy cd) <> " NULL"

data Default (def :: Symbol) cd
instance defaultType :: (ColumnType cd i o, IsSymbol a) => ColumnType (Default a cd) (Maybe i) o where
    createType _ = createType proxy <> " DEFAULT " <> reflectSymbol sproxy
      where
        proxy = Proxy :: Proxy cd
        sproxy = SProxy :: SProxy a
