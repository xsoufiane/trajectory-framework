{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Event.EventTime (EventTime ((<), (<=), (>), (>=))) where

import Prelude hiding ((<), (<=))
import Data.Time (ChrononObs, Period)
import Data.Period.PeriodObs (PeriodObs)

import Data.Event.EventAlgebra (Event, EventAlgebra)
import Data.Event.SemanticEventAlgebra (SemanticEvent, SemanticEventAlgebra)

import qualified Data.Event.EventAlgebra as E (EventAlgebra(time))
import qualified Data.Period.PeriodObs as P (PeriodObs((<)))
import qualified Data.Event.SemanticEventAlgebra as SE (SemanticEventAlgebra(time))
import qualified Data.Time as C (ChrononObs((<)))
  
---------------------------------------

-- | Observations  
class EventTime e where
    (<) :: e -> e -> Bool
  
    (>) :: e -> e -> Bool   
    x > y = y < x
    
    (<=) :: Eq e => e -> e -> Bool   
    x <= y = x < y || x == y
    
    (>=) :: Eq e => e -> e -> Bool   
    x >= y = y <= x

instance (ChrononObs t, EventAlgebra l t) => EventTime (Event l t) where
    x < y = (C.<) (E.time x) (E.time y)

instance (ChrononObs t, SemanticEventAlgebra l s t) => EventTime (SemanticEvent l s t) where
    x < y = (C.<) (SE.time x) (SE.time y)

instance {-# OVERLAPPING #-} (PeriodObs c t, EventAlgebra l (Period c t)) => EventTime (Event l (Period c t)) where
    x < y = (P.<) (E.time x) (E.time y)

instance {-# OVERLAPPING #-} (PeriodObs c t, SemanticEventAlgebra l  s (Period c t)) 
    => EventTime (SemanticEvent l s (Period c t)) where
    
    x < y = (P.<) (SE.time x) (SE.time y)    
