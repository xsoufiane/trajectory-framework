{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Event.EventAlgebra
    (
      -- * Types
      Event
    , EventAlgebra

      -- * Constructors
    , construct
    , mapObservable

      -- * Observations
    , observables
    , time
    ) where

import Data.HList
import Data.Kind (Type)
import Data.Time (Time)
import Prelude hiding ((<))
import Structure.Identity (Identity)

import Data.Internal (HObservable)

----------------------------------------------------------------------------------------------------------------

-- | Algebra
class
    (
      Functor (Event l)
    , HObservable l
    , Time t
    , Identity (Event l t)
    , Semigroup (Event l t)
    ) => EventAlgebra (l :: [Type]) t where
    data Event l t

    -- | Constructors
    construct :: HList l -> t -> Event l t

    -- | Observable related constructors
    mapObservable :: EventAlgebra l' t => (HList l -> HList l') -> Event l t -> Event l' t
    mapObservable f e = construct (f $ observables e) (time e)

    -- | Observations
    observables :: Event l t -> HList l
    time :: Event l t -> t
