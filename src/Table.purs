module Table where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Exec (Runner, AffOrm)
import Type.Prelude (Proxy)

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
