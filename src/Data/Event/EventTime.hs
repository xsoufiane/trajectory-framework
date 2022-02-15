{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Event.EventTime (EventTime((<), (<=), (>), (>=), betweenness)) where

import Data.Chronon (Chronon)
import Data.Period (Period)
import Prelude hiding ((<), (<=))

import Data.Event.EventAlgebra (Event, EventAlgebra)
import Data.Event.SemanticEventAlgebra (SemanticEvent, SemanticEventAlgebra)

import qualified Data.Event.EventAlgebra as E (EventAlgebra(time))
import qualified Data.Event.SemanticEventAlgebra as SE (SemanticEventAlgebra(time))
import qualified Relation.Order as Time (Order((<)))

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
    
    -- | x is between y z
    betweenness :: e -> e -> e -> Bool
    betweenness x y z = (y < x && x < z) || (z < x && x < y)

-- | Useful Instances 
instance (Chronon t, Time.Order t, EventAlgebra l t) => EventTime (Event l t) where
    x < y = (Time.<) (E.time x) (E.time y)

instance (Chronon t, Time.Order t, SemanticEventAlgebra l s t) => EventTime (SemanticEvent l s t) where
    x < y = (Time.<) (SE.time x) (SE.time y)

instance {-# OVERLAPPING #-} (Time.Order (Period c t), EventAlgebra l (Period c t))
    => EventTime (Event l (Period c t))
  where
    x < y = (Time.<) (E.time x) (E.time y)

instance {-# OVERLAPPING #-} (Time.Order (Period c t), SemanticEventAlgebra l s (Period c t)) 
    => EventTime (SemanticEvent l s (Period c t)) 
  where
    x < y = (Time.<) (SE.time x) (SE.time y)
