{-# LANGUAGE DataKinds #-}
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

      -- * Observations
    , observables
    , time
    , (===)
    , EventChrononObs((<))
    ) where

import Data.HList (HList)
import Data.Kind (Type)
import Prelude hiding ((<))

import Data.Internal.HObservable (HObservable)
import Data.Time (Time, ChrononObs, Period)

import qualified Data.Chronon as C (ChrononObs((<)))

---------------------------------------------------------------------------------

-- | Algebra
class (HObservable l, Time t) => EventAlgebra (l :: [Type]) t where
    data Event l t

    -- | Constructors
    construct :: HList l -> t -> Event l t

    -- | Observations
    observables :: Event l t -> HList l
    time :: Event l t -> t
    (===) :: Event l t -> Event l t -> Bool
 
class (EventAlgebra (l :: [Type]) t, ChrononObs t) => EventChrononObs (l :: [Type]) t where 
    (<) :: Event l t -> Event l t -> Bool
    x < y =  (C.<) (time x) (time y)
