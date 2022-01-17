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
    
      -- * Observations
    , elements
    , semanticAnnotation
    ) where

import Data.Annotation.SemanticAnnotation (SemanticAnnotation)
import Data.Event.EventAlgebra (Event)
import Data.Event.SemanticEventAlgebra (SemanticEvent)
import Data.Episode.EpisodeAlgebra (Episode)
import Data.Episode.SemanticEpisodeAlgebra (SemanticEpisode)

---------------------------------------

-- | Internal
class InternalElement e
instance InternalElement (Event l t)
instance InternalElement (SemanticEvent l s t)
instance InternalElement (Episode a l t)
instance InternalElement (SemanticEpisode a e s)

-- | Algebra
class (Functor (SemanticTrajectory e), InternalElement e, SemanticAnnotation s) => SemanticTrajectoryAlgebra e s where
    data SemanticTrajectory e s
    
    -- | Constructors
    construct :: [e] -> s -> SemanticTrajectory e s 
     
    -- | Observations
    elements :: SemanticTrajectory e s -> [e]
    semanticAnnotation :: SemanticTrajectory e s -> s
