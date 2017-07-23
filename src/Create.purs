module Create where

import Prelude

import Column (class ColumnType, And, Column, createType)
import Connection (ORM, query)
import Control.Monad.Aff (Aff)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Table (Table)
import Type.Proxy (Proxy(..))

class CreateRecord cd where
  toCreate :: Proxy cd -> String

instance columnCreate :: (IsSymbol n, ColumnType cd i o) => CreateRecord (Column n cd) where
  toCreate _ = reflectSymbol (SProxy :: SProxy n) <> " " <> createType (Proxy :: Proxy cd)

instance andCreate :: (CreateRecord c1, CreateRecord c2) => CreateRecord (And c1 c2) where
  toCreate _ = toCreate (Proxy :: Proxy c1) <> ", " <> toCreate (Proxy :: Proxy c2)

create :: forall n cd fx. IsSymbol n => CreateRecord cd => Proxy (Table n cd) -> Aff (orm :: ORM | fx) Unit
create _ = query sql [] $> unit
  where
    name = reflectSymbol (SProxy :: SProxy n)
    sql = "CREATE TABLE " <> name <> " (" <> toCreate (Proxy :: Proxy cd) <> ")"

