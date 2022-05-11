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
import Data.Period (Period, PeriodType(Closed))
import Data.Period.PeriodObs (PeriodObs(starts, finishes, during, overlaps), Meets(meets))
import Data.Time (Time)
import Prelude hiding ((<))
import Relation.Identity (Identity)
import Relation.Order (Order((<)))

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

-- | Useful Instances for time observations
instance (EventAlgebra l t, Order t) => Order (Event l t) where
   e < e' = time e < time e'

instance (EventAlgebra l (Period c t), PeriodObs (Period c t)) => PeriodObs (Event l (Period c t)) where
    starts e e' = starts (time e) (time e')

    finishes e e' = finishes (time e) (time e')

    during e e' = during (time e) (time e')

    overlaps e e' = overlaps (time e) (time e')

instance (EventAlgebra l (Period 'Closed t), Meets (Period 'Closed t)) => Meets (Event l (Period 'Closed t)) where
    meets e e'= meets (time e) (time e')
                                                                                                                                                                                                                                                                                                                                                                                                                                        