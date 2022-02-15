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

      -- * Observations
    , observables
    , time
    , (===)
    ) where

import Data.HList (HList)
import Data.Kind (Type)
import Data.Period (Period, PeriodType(Closed))
import Data.Period.PeriodObs (PeriodObs(starts, finishes, during, overlaps), Meets(meets))
import Data.Time (Time)
import Prelude hiding ((<))

import Data.Internal.HObservable (HObservable)

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

-- | Useful Instances
instance PeriodObs (Period c t) => PeriodObs (Event l (Period c t)) where
    starts = starts
    
    finishes = finishes
    
    during = during
    
    overlaps = overlaps

instance Meets (Period 'Closed t) => Meets (Event l (Period 'Closed t)) where
    meets = meets
                                                                                                                                                                                                                                                                                                                                                                                                                                        