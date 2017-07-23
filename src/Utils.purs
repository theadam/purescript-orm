module Utils where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Number as Number
import Data.Number.Format (toString)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

data Selectable (n :: Symbol) t = Selectable String

toSelect :: forall n t. IsSymbol n => Selectable n t -> String
toSelect (Selectable alias) = alias <> "." <> reflectSymbol (SProxy :: SProxy n)

class Sqlable a where
  toSql :: a -> String
  fromSql :: String -> a

instance stringSql :: Sqlable String where
  toSql s = "'" <> s <> "'"
  fromSql s = replaceAll (Pattern "^'|'$") (Replacement "") s

instance numberSql :: Sqlable Number where
  toSql = toString
  fromSql = fromMaybe 0.0 <<< Number.fromString

instance intSql :: Sqlable Int where
  toSql = toStringAs decimal
  fromSql = fromMaybe 0 <<< Int.fromString

instance selectableSql :: (IsSymbol col) => Sqlable (Selectable col t) where
  toSql s = toSelect s
  fromSql = Selectable

