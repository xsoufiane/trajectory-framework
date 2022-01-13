{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TrajectoryAlgebra
    (
      -- * Types
      TrajectoryAlgebra
    , Trajectory

      -- * Constructors
    , construct
    
      -- * Observations
    , elements
    , semanticAnnotation
    ) where

import Data.EventAlgebra (Event)
import Data.EpisodeAlgebra (Episode)
import Data.Semantic.SemanticAnnotation (SemanticAnnotation)
import Data.Semantic.SemanticEventAlgebra (SemanticEvent)
import Data.Semantic.SemanticEpisodeAlgebra (SemanticEpisode)

---------------------------------------

-- | Internal
class InternalElement e
instance InternalElement (Event l t)
instance InternalElement (SemanticEvent l s t)
instance InternalElement (Episode a l t)
instance InternalElement (SemanticEpisode a l t)

-- | Algebra
class (InternalElement e, SemanticAnnotation s) => TrajectoryAlgebra e s where
    data Trajectory e s
    
    -- | Constructors
    construct :: [e] -> s -> Trajectory e s 
     
    -- | Observations
    elements :: Trajectory e s -> [e]
    semanticAnnotation :: Trajectory e s -> s
