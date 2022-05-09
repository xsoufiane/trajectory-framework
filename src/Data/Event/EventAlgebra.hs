{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Event.EventAlgebra
    (
      -- * Types
      Event
    , EventAlgebra

      -- * Constructors
    , construct
    , timeEvent
    , singleton
    , append
    , prepend
    , appendL
    , prependL
    , appendE
    , prependE
    , replace
    , delete
    , mapObservable

    , updateTime
    , update

      -- * Observations
    , observables
    , time
    ) where

import Control.Exception.Base (Exception, displayException)
import Data.HList
import Data.Kind (Type)
import Data.Observable (Observable)
import Data.Period (Period, PeriodType(Closed))
import Data.Period.PeriodObs (PeriodObs(starts, finishes, during, overlaps), Meets(meets))
import Relation.Identity (Identity)
import Relation.Order (Order((<)))
import Data.Time (Time)
import Prelude hiding ((<))

import Data.Internal.HObservable (HObservable)

---------------------------------------------------------------------------------

-- | Exceptions
data TimeNotIdentical = TimeNotIdentical deriving (Show)
data ObservableNotFound = ObservableNotFound deriving (Show)

instance Exception TimeNotIdentical where
    displayException _ = "Time must be identical!!!"

instance Exception ObservableNotFound where
    displayException _ = "Observable not found in event!!!"

-- | Internal
type family Contains e (l :: [Type]) :: Bool where
    Contains e '[] = 'False
    Contains e (e : l) = 'True
    Contains e (o : l) = Contains e l

-- | Algebra
class (Functor (Event l), HObservable l, Time t, Identity (Event l t)) => EventAlgebra (l :: [Type]) t where
    data Event l t

    -- | Constructors
    construct :: HList l -> t -> Event l t
    
    timeEvent :: EventAlgebra '[] t => t -> Event '[] t
    timeEvent t = construct HNil t
    
    singleton :: EventAlgebra '[o] t => Observable o => t -> o -> Event '[o] t
    singleton t o = construct (o `HCons` HNil) t
    
    -- | Observable related constructors
    append :: Observable o => o -> Event l t -> Event (HAppendListR l '[o]) t
    prepend :: Observable o => o -> Event l t -> Event (o ': l) t
    appendL :: HObservable l' => HList l' -> Event l t -> Event (HAppendListR l l') t
    prependL :: HObservable l' => HList l' -> Event l t -> Event (HAppendListR l' l) t
    appendE :: Identity t => Event l t -> Event l' t -> Either TimeNotIdentical (Event (HAppendListR l l') t)
    prependE :: Identity t => Event l t -> Event l' t -> Either TimeNotIdentical (Event (HAppendListR l' l) t)
    replace :: (Contains o l ~ 'True, Observable o) => o -> Event l t -> Either ObservableNotFound (Event l' t)
    delete :: (Contains o l ~ 'True ,Observable o) => o -> Event l t -> Either ObservableNotFound (Event l' t)      
    
    mapObservable ::  EventAlgebra l' t => (HList l -> HList l') -> Event l t -> Event l' t
    mapObservable f e = construct (f $ observables e) (time e)
    
    update :: EventAlgebra l' t  => HList l' -> Event l t -> Event l' t
    update l' e = mapObservable (const l') e
    
    -- | Time related constructors    
    updateTime :: Time t' => t' -> Event l t -> Event l t'
    updateTime = (<$)
    
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
                                                                                                                                                                                                                                                                                                                                                                                                                                        