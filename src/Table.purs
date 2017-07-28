module Table where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Exec (Runner, AffOrm)
import Type.Prelude (Proxy(..))

-- cs is a column descriptor or descriptors
data Table (name :: Symbol) cs

drop :: forall n cd fx. IsSymbol n => Runner fx -> Proxy (Table n cd) -> AffOrm fx Unit
drop runner _ = runner sql [] $> unit
  where
    name = reflectSymbol (SProxy :: SProxy n)
    sql = "DROP TABLE " <> name

truncate :: forall n cd fx. IsSymbol n => Runner fx -> Proxy (Table n cd) -> AffOrm fx Unit
truncate runner _ = runner sql [] $> unit
  where
    name = reflectSymbol (SProxy :: SProxy n)
    sql = "TRUNCATE TABLE " <> name

tableName :: forall n cd. IsSymbol n => Proxy (Table n cd) -> String
tableName _ = reflectSymbol (SProxy :: SProxy n)

columns :: forall n cd. Proxy (Table n cd) -> Proxy cd
columns _ = Proxy
