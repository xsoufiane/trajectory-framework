{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Episode.EpisodeAlgebra
    (
      -- * Types
      Episode
    , EpisodeAlgebra

      -- * Constructors
    , construct
    , mapAnnotations
    
      -- * Observations
    , events
    , annotations
    ) where

import Data.HList (HList)
import Data.Kind (Type)

import Data.Episode.Internal (HAnnotation)
import Data.Event.EventAlgebra (Event)

-------------------------------------------------------------------------------------------------------

-- | Internal
class InternalEvent e
instance InternalEvent (Event l t)

-- | Algebra
class (Functor (Episode a), HAnnotation a, InternalEvent e) => EpisodeAlgebra (a :: [Type]) e where
    data Episode a e
    
    -- | Constructors
    construct :: HList a -> [e] -> Episode a e
    
    -- | Annotation related constructors
    mapAnnotations ::  EpisodeAlgebra a' e => (HList a -> HList a') -> Episode a e -> Episode a' e
    mapAnnotations f e = construct (f $ annotations e) (events e)

    -- | Observations
    events :: Episode a e -> [e]
    annotations :: Episode a e -> HList a
