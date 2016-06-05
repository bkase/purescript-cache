module Main where

import Prelude ((<>), Unit, unit, pure, class Semigroup, bind, (*>), id, const, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Monoid (class Monoid, mempty)
import Control.MonadPlus (class MonadPlus, empty, (<|>))

data Cache m k v = Cache { get :: k -> m v
                         , set :: k -> v -> m Unit }

instance semigroupCache :: (MonadPlus m) => Semigroup (Cache m k v) where
  append (Cache{get, set}) (Cache{get: get', set: set'}) =
    Cache
    { get: \k -> get k <|> do
              v <- get' k
              set k v
              pure v
    , set: \k -> \v -> do
              set k v *> set' k v
    }

instance monoidCache :: (MonadPlus m) => Monoid (Cache m k v) where
  mempty = Cache { get: const empty
                 , set: const $ const $ pure unit }

compoundCache :: forall m. (MonadPlus m) => Cache m Int String
compoundCache = mempty <> mempty

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello world"
