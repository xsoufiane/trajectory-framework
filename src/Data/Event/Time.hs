{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Event.Time (EventTimeObs ((<), (>))) where

import Prelude hiding ((<), (>))  

import Data.Event.EventAlgebra (Event)
import Data.Event.SemanticEventAlgebra (SemanticEvent)
  
---------------------------------------

-- | Internal
type family EventType e :: Bool where
    EventType (Event l t) = 'True
    EventType (SemanticEvent l s t) = 'True
  
-- | Observations  
class EventType e ~ 'True => EventTimeObs e where
    (<) :: e -> e -> Bool
  
    (>) :: e -> e -> Bool   
    x > y = y < x
