module Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Data.Array (cons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Exception (Error, error)
import Foreign (F, Foreign, isNull, readBoolean, readInt, readString)
import Prim.RowList (Nil, Cons, class RowToList)
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)

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

convertF :: forall a. F a -> Either Error a
convertF = lmap (error <<< show) <<< runExcept

class FromForeign a where
  fromForeign :: Foreign -> Either Error a

instance fromForeignInt :: FromForeign Int where
  fromForeign = convertF <<< readInt

instance fromForeignString :: FromForeign String where
  fromForeign = convertF <<< readString

instance fromForeignBoolean :: FromForeign Boolean where
  fromForeign = convertF <<< readBoolean

instance fromForeignMaybe :: (FromForeign a) => FromForeign (Maybe a) where
  fromForeign a
    | isNull a = pure Nothing
    | otherwise = Just <$> fromForeign a

convertEither :: forall a b m. MonadThrow b m => Either b a -> m a
convertEither (Right a) = pure a
convertEither (Left b) = throwError b

foreign import stringify :: forall a. a -> String
foreign import null :: Foreign
