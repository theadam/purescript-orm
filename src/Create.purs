module Create where

import Prelude

import Column (class ColumnType, And, Column, createType)
import Control.Monad.Aff (Aff)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Exec (ORM, Runner)
import Table (Table)
import Type.Proxy (Proxy(..))

class CreateRecord cd where
  toCreate :: Proxy cd -> String

instance columnCreate :: (IsSymbol n, ColumnType cd i o) => CreateRecord (Column n cd) where
  toCreate _ = reflectSymbol (SProxy :: SProxy n) <> " " <> createType (Proxy :: Proxy cd)

instance andCreate :: (CreateRecord c1, CreateRecord c2) => CreateRecord (And c1 c2) where
  toCreate _ = toCreate (Proxy :: Proxy c1) <> ", " <> toCreate (Proxy :: Proxy c2)

createBase :: forall n cd fx. IsSymbol n => CreateRecord cd => Runner fx -> String -> Proxy (Table n cd) -> Aff (orm :: ORM | fx) Unit
createBase runner params _ = runner sql [] $> unit
  where
    name = reflectSymbol (SProxy :: SProxy n)
    sql = "CREATE TABLE " <> params <> name <> " (" <> toCreate (Proxy :: Proxy cd) <> ")"

create :: forall n cd fx. IsSymbol n => CreateRecord cd => Runner fx -> Proxy (Table n cd) -> Aff (orm :: ORM | fx) Unit
create runner = createBase runner ""

createIfNotExists :: forall n cd fx. IsSymbol n => CreateRecord cd => Runner fx -> Proxy (Table n cd) -> Aff (orm :: ORM | fx) Unit
createIfNotExists runner = createBase runner "IF NOT EXISTS "
