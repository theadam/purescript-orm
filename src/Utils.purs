module Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList (Nil, Cons, class RowToList)
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)

data ColumnAlias (columnName :: Symbol) o = ColumnAlias String

toParam :: Int -> String
toParam i = "$" <> show i

class Keys rl where
    keys :: rl -> Array String
instance rowKeys :: (RowToList r rl, Keys (RLProxy rl)) => Keys (RProxy r) where
    keys _ = keys (RLProxy :: RLProxy rl)
else instance nilKeys :: Keys (RLProxy Nil) where
    keys _ = []
else instance consKeys :: (Keys (RLProxy rl'), IsSymbol k) => Keys (RLProxy (Cons k v rl')) where
    keys _ = cons (reflectSymbol (SProxy :: SProxy k)) (keys (RLProxy :: RLProxy rl'))
else instance recordKeys :: (RowToList r rl, Keys (RLProxy rl)) => Keys (Proxy (Record r)) where
    keys _ = keys (RLProxy :: RLProxy rl)

convertEither :: forall a b m. MonadThrow b m => Either b a -> m a
convertEither (Right a) = pure a
convertEither (Left b) = throwError b

foreign import stringify :: forall a. a -> String
