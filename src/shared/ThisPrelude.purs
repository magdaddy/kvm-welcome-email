module ThisPrelude
  ( module Control.Applicative
  , module Control.Apply
  , module Control.Bind
  , module Control.Category
  , module Control.Monad
  , module Control.Semigroupoid
  , module Data.Boolean
  , module Data.BooleanAlgebra
  , module Data.Bounded
  , module Data.CommutativeRing
  , module Data.DivisionRing
  , module Data.Eq
  , module Data.EuclideanRing
  , module Data.Field
  , module Data.Function
  , module Data.Functor
  , module Data.HeytingAlgebra
  , module Data.Monoid
  , module Data.NaturalTransformation
  , module Data.Ord
  , module Data.Ordering
  , module Data.Ring
  , module Data.Semigroup
  , module Data.Semiring
  , module Data.Show
  , module Data.Unit
  , module Data.Void
  -- My Stuff --
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Data.Bifunctor
  , module Data.Either
  , module Data.Maybe
  , module Data.Newtype
  , module Effect
  , module Effect.Aff
  , module Effect.Aff.Class
  , module Effect.Class
  , log
  ) where

import Control.Applicative (class Applicative, pure, liftA1, unless, when)
import Control.Apply (class Apply, apply, (*>), (<*), (<*>))
import Control.Bind (class Bind, bind, class Discard, discard, ifM, join, (<=<), (=<<), (>=>), (>>=))
import Control.Category (class Category, identity)
import Control.Monad (class Monad, liftM1, unlessM, whenM, ap)
import Control.Semigroupoid (class Semigroupoid, compose, (<<<), (>>>))

import Data.Boolean (otherwise)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.Bounded (class Bounded, bottom, top)
import Data.CommutativeRing (class CommutativeRing)
import Data.DivisionRing (class DivisionRing, recip)
import Data.Eq (class Eq, eq, notEq, (/=), (==))
import Data.EuclideanRing (class EuclideanRing, degree, div, mod, (/), gcd, lcm)
import Data.Field (class Field)
import Data.Function (const, flip, ($), (#))
import Data.Functor (class Functor, flap, map, void, ($>), (<#>), (<$), (<$>), (<@>))
import Data.HeytingAlgebra (class HeytingAlgebra, conj, disj, not, (&&), (||))
import Data.Monoid (class Monoid, mempty)
import Data.NaturalTransformation (type (~>))
import Data.Ord (class Ord, compare, (<), (<=), (>), (>=), comparing, min, max, clamp, between)
import Data.Ordering (Ordering(..))
import Data.Ring (class Ring, negate, sub, (-))
import Data.Semigroup (class Semigroup, append, (<>))
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)

-- My Stuff --

import Control.Monad.Except (ExceptT, except, runExceptT, withExceptT)
import Control.Monad.Reader (class MonadAsk, ask, asks, runReaderT)
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..), note)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

import Effect.Console (log) as Console

log :: forall m. MonadEffect m => String -> m Unit
log = liftEffect <<< Console.log

