{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Episode
    (
      -- * Types
      Episode
    , EpisodeAlgebra
      
      -- * Observations
    , events
    , annotation
    ) where

import Data.Annotation (Annotation)
import Data.Event (Event)
import Data.Time (Time)
  
---------------------------------------

class (Annotation a, Time t) => EpisodeAlgebra a l t where
    data Episode a l t
    
    episode :: a -> [Event l t] -> Episode a l t
    
    events :: Episode a l t -> [Event l t]
    annotation :: Episode a l t -> a
