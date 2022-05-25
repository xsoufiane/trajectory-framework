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
    , append
    , prepend
    , mapObservable

      -- * Observations
    , observables
    , time
    ) where

import Data.HList
import Data.Kind (Type)
import Data.Time (Time)
import Prelude hiding ((<))
import Relation.Identity (Identity)

import Data.Event.Internal (HObservable)

----------------------------------------------------------------------------------------------------------------

-- | Algebra
class (Functor (Event l), HObservable l, Time t, Identity (Event l t)) => EventAlgebra (l :: [Type]) t where
    data Event l t

    -- | Constructors
    construct :: HList l -> t -> Event l t

    -- | Observable related constructors
    append :: HObservable l' => HList l' -> Event l t -> Event (HAppendListR l l') t
    prepend :: HObservable l' => HList l' -> Event l t -> Event (HAppendListR l' l) t

    mapObservable ::  EventAlgebra l' t => (HList l -> HList l') -> Event l t -> Event l' t
    mapObservable f e = construct (f $ observables e) (time e)

    -- | Observations
    observables :: Event l t -> HList l
    time :: Event l t -> t
