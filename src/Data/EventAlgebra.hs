{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Event
    (
      -- * Types
      Event
    , EventAlgebra
      
      -- * Observations
    ,  observables
    , time
    ) where

import Data.HList (HList)
import Data.Kind (Type)

import Data.Observable (Observable)
import Data.Time (Time)

-- | Internal
class HObservable (l :: [Type])
instance (Observable o, HObservable l) => HObservable (o ': l)
instance {-# OVERLAPPING #-} Observable o => HObservable (o ': '[])

-- | observations
class (HObservable l, Time t) => EventAlgebra (l :: [Type]) t where
    data Event l t

    observables :: Event l t -> HList l
    time :: Event l t -> t
  