module Column where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Database.PostgreSQL (class FromSQLValue, class ToSQLValue, null)
import Type.Proxy (Proxy(..))

data And c1 c2
infixr 7 type And as &

data IdType
type Id = Column "id" IdType

instance idTypeToSQL :: ToSQLValue IdType where
  toSQLValue _ = null

instance idColumnType :: ColumnType IdType (Maybe IdType) Int where
  createType _ = "SERIAL PRIMARY KEY"

foreign import kind TBool
foreign import data TTrue :: TBool
foreign import data TFalse :: TBool

class (ToSQLValue i, FromSQLValue o) <= ColumnType cd i o | cd -> i, cd -> o where
  createType :: Proxy cd -> String

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

-- cd is a column descriptor
data Column (name :: Symbol) cd

data NotNull cd
instance notNullType :: (ColumnType cd i o) => ColumnType (NotNull cd) i o where
  createType _ = createType (Proxy :: Proxy cd) <> " NOT NULL"

data Nullable cd
instance nullType :: (ColumnType cd i o) => ColumnType (Nullable cd) (Maybe i) (Maybe o) where
  createType _ = createType (Proxy :: Proxy cd) <> " NULL"

data Default (def :: Symbol) cd
instance defaultType ::
  (ColumnType cd i o, IsSymbol a) =>
  ColumnType (Default a cd) (Maybe i) o where
    createType _ = createType proxy <> " DEFAULT " <> reflectSymbol sproxy
      where
        proxy = Proxy :: Proxy cd
        sproxy = SProxy :: SProxy a

