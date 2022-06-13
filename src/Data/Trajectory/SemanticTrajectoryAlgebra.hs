{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trajectory.SemanticTrajectoryAlgebra
    (
      -- * Types
      SemanticTrajectoryAlgebra
    , SemanticTrajectory

      -- * Constructors
    , construct
    , mapSemanticAnnotations
    
      -- * Observations
    , semanticAnnotations
    ) where

import Data.HList (HList)
import Data.Kind (Type)
import Prelude hiding (filter, head, tail)
import Relation.Identity (Identity)

import Data.Event.EventAlgebra (Event)
import Data.Event.SemanticEventAlgebra (SemanticEvent)
import Data.Episode.EpisodeAlgebra (Episode)
import Data.Episode.SemanticEpisodeAlgebra (SemanticEpisode)
import Data.Internal (HSemanticAnnotation)
import Data.TrajectoryLike

------------------------------------------------------------------------------

-- | Internal
class InternalElement e
instance InternalElement (Event l t)
instance InternalElement (SemanticEvent l s t)
instance InternalElement (Episode a e)
instance InternalElement (SemanticEpisode a e s)

-- | Algebra
class
    (
      Functor (SemanticTrajectory s)
    , Identity (SemanticTrajectory s e)
    , InternalElement e
    , HSemanticAnnotation s
    , Semigroup (SemanticTrajectory s e)
    , TrajectoryLike (SemanticTrajectory s e) e
    ) => SemanticTrajectoryAlgebra (s :: [Type]) e
  where
    data SemanticTrajectory s e
    
    -- | Constructors
    construct :: [e] -> HList s -> SemanticTrajectory s e

    mapSemanticAnnotations
        :: SemanticTrajectoryAlgebra s' e
        => (HList s -> HList s') -> SemanticTrajectory s e -> SemanticTrajectory s' e
    mapSemanticAnnotations f e = construct (elements e) (f $ semanticAnnotations e)

    -- | Observations
    semanticAnnotations :: SemanticTrajectory s e -> HList s
