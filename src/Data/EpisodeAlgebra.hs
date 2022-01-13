{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.EpisodeAlgebra
    (
      -- * Types
      Episode
    , EpisodeAlgebra

      -- * Constructors
    , construct
    
      -- * Observations
    , events
    , annotation
    ) where

import Data.Annotation (Annotation)
import Data.EventAlgebra (Event)
import Data.Time (Time)

---------------------------------------

class (Annotation a, Time t) => EpisodeAlgebra a l t where
    data Episode a l t
    
    -- | Constructors
    construct :: a -> [Event l t] -> Episode a l t

    -- | Observations
    events :: Episode a l t -> [Event l t]
    annotation :: Episode a l t -> a
