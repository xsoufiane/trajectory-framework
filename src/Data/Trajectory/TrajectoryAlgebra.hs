{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trajectory.TrajectoryAlgebra
    (
      -- * Types
      TrajectoryAlgebra
    , Trajectory

      -- * Constructors
    , construct
    
      -- * Observations
    , elements
    ) where

import Data.Event.EventAlgebra (Event)
import Data.Episode.EpisodeAlgebra (Episode)

---------------------------------------

-- | Internal
class InternalElement e
instance InternalElement (Event l t)
instance InternalElement (Episode a l t)

-- | Algebra
class InternalElement e => TrajectoryAlgebra e where
    data Trajectory e
    
    -- | Constructors
    construct :: [e] -> Trajectory e
     
    -- | Observations
    elements :: Trajectory e -> [e]
