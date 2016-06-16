module Main where

import Prelude ((<>), Unit, unit, pure, class Semigroup, bind, (*>), id, const, ($), (<$>), (<<<), flip, (+))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Monoid (class Monoid, mempty)
import Data.Profunctor
import Data.Functor.Contravariant (class Contravariant, (>$<))
import Data.Functor
import Control.MonadPlus (class MonadPlus, empty, (<|>))

type Cache m k v = Cache_ m k v v

data Cache_ m k v w = Cache { get :: k -> m w
                            , set :: k -> v -> m Unit }

data Flipped p (m :: * -> *) v w k = Flipped (p m k v w)

unflip :: forall p m v w k. (Functor m) => Flipped p m v w k -> p m k v w
unflip (Flipped c) = c

instance semigroupCache :: (MonadPlus m) => Semigroup (Cache_ m k v v) where
  append (Cache{get, set}) (Cache{get: get', set: set'}) =
    Cache
    { get: \k -> get k <|> do
              v <- get' k
              set k v
              pure v
    , set: \k -> \v -> do
              set k v *> set' k v
    }

instance monoidCache :: (MonadPlus m) => Monoid (Cache_ m k v v) where
  mempty = Cache { get: const empty
                 , set: const $ const $ pure unit }

instance profunctorCache :: (Functor m) => Profunctor (Cache_ m k) where
  dimap f g (Cache{get, set}) =
    Cache
    { get: \k -> g <$> get k
    , set: \k -> \v -> set k (f v)
    }

instance contravariantCache :: (Functor m) => Contravariant (Flipped Cache_ m v w) where
  cmap f (Flipped(Cache{get, set})) =
    Flipped $ Cache { get: get <<< f
                    , set: \k -> \v -> set (f k) v
                    }

compoundCache :: forall m. (MonadPlus m) => Cache m Int String
compoundCache = (unflip ((_+1) >$< Flipped(mempty))) <> dimap vf vf mempty
  where vf = (_ <> "!")

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello world"
