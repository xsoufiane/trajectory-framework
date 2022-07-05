{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Trajectory.SemanticTrajectoryAlgebra
    (
      -- * Types
      SemanticTrajectoryAlgebra
    , SemanticTrajectory

      -- * Constructors
    , construct
    , enrich
    , mapSemanticAnnotations
    
      -- * Observations
    , semanticAnnotations
    , deannotate
    ) where

import Data.HList (HList)
import Data.Kind (Type)
import Prelude hiding (filter, head, tail)
import Structure.Identity (Identity)

import Data.Event.EventAlgebra (Event)
import Data.Event.SemanticEventAlgebra (SemanticEvent)
import Data.Episode.EpisodeAlgebra (Episode)
import Data.Episode.SemanticEpisodeAlgebra (SemanticEpisode)
import Data.Internal (HSemanticAnnotation, NotEmpty)
import Data.TrajectoryLike
import Data.Trajectory.TrajectoryAlgebra (Trajectory)

------------------------------------------------------------------------------

-- | Internal
class InternalElement e
instance InternalElement (Event l t)
instance InternalElement (SemanticEvent l s t)
instance InternalElement (Episode a e)
instance InternalElement (SemanticEpisode a s e)

type family DeAnnotate e where
    DeAnnotate (Event l t) = Event l t
    DeAnnotate (SemanticEvent l s t) = Event l t
    DeAnnotate (Episode a e) = Episode a e
    DeAnnotate (SemanticEpisode a s e) = Episode a e

type family Semantic e :: Bool where
    Semantic (SemanticEvent l s t) = 'True
    Semantic (SemanticEpisode a s e) = 'True
    Semantic _ = 'False
    
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
    construct :: Semantic e ~ 'True => [e] -> HList s -> SemanticTrajectory s e
    enrich ::  NotEmpty s ~ 'True => Trajectory e -> HList s -> SemanticTrajectory s e

    mapSemanticAnnotations
        :: SemanticTrajectoryAlgebra s' e
        => (HList s -> HList s') -> SemanticTrajectory s e -> SemanticTrajectory s' e

    -- | Observations
    semanticAnnotations :: SemanticTrajectory s e -> HList s
    deannotate :: SemanticTrajectory s e -> Trajectory (DeAnnotate e)
