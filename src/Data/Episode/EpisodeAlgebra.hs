{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Episode.EpisodeAlgebra
    (
      -- * Types
      Episode
    , EpisodeAlgebra

      -- * Constructors
    , construct
    , mapAnnotations
    
      -- * Observations
    , annotations
    ) where

import Data.HList (HList)
import Data.Kind (Type)
import Prelude hiding (filter, head, tail)
import Structure.Identity (Identity)

import Data.Episode.Internal (HAnnotation)
import Data.Event.EventAlgebra (Event)
import Data.Internal (NotEmpty)
import Data.TrajectoryLike

-------------------------------------------------------------------------------------------------------

-- | Internal
class InternalEvent e
instance InternalEvent (Event l t)

-- | Algebra
class 
    (
      Functor (Episode a)
    , HAnnotation a
    , Identity (Episode a e)
    , InternalEvent e
    , Semigroup (Episode a e)
    , TrajectoryLike (Episode a e) e
    , NotEmpty a ~ 'True
    ) => EpisodeAlgebra (a :: [Type]) e 
  where
    data Episode a e
    
    -- | Constructors
    construct :: HList a -> [e] -> Episode a e

    -- | Annotation related constructors
    mapAnnotations :: EpisodeAlgebra a' e => (HList a -> HList a') -> Episode a e -> Episode a' e
    mapAnnotations f e = construct (f $ annotations e) (elements e)

    -- | Observations
    annotations :: Episode a e -> HList a
