{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trajectory.TrajectoryAlgebra
    (
      -- * Trajectory Types
      TrajectoryAlgebra
    , Trajectory

      -- * Constructors
    , construct
    , fromEpisode
    , structure
    , unStructure
    
      -- * Observations
    , toEpisode
    ) where

import Data.HList (HList)
import Prelude hiding (filter, head, tail)
import Structure.Identity (Identity)

import Data.Event.EventAlgebra (Event)
import Data.Episode.EpisodeAlgebra (Episode, EpisodeAlgebra)
import Data.TrajectoryLike

import qualified Data.Episode.EpisodeAlgebra as Episode (construct) 

---------------------------------------

-- | Internal
class InternalElement e
instance InternalElement (Event l t)
instance InternalElement (Episode a e)

-- | Algebra
class 
    (
      Functor Trajectory
    , Identity (Trajectory e)
    , InternalElement e
    , Semigroup (Trajectory e)
    , TrajectoryLike (Trajectory e) e
    ) => TrajectoryAlgebra e 
  where
    data Trajectory e
    
    -- | Constructors
    construct :: [e] -> Trajectory e
    
    fromEpisode :: EpisodeAlgebra a e => Episode a e -> Trajectory e
    fromEpisode = construct . elements 
        
    structure :: e ~ Event l t => (e -> HList a) -> Trajectory e -> Trajectory (Episode a e)
    unStructure :: e ~ Event l t => Trajectory (Episode a e) -> Trajectory e
     
    -- | Observations
    toEpisode :: EpisodeAlgebra a e => HList a -> Trajectory e -> Episode a e
    toEpisode a t = Episode.construct a (elements t)
