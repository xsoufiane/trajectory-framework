{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trajectory.TrajectoryAlgebra
    (
      -- * Types
      TrajectoryAlgebra
    , Trajectory
    , SimilarityValue

      -- * Constructors
    , construct
    , fromEpisode
    , before
    , after
    , during
    , tail
    , append
    , prepend
    , filter 
    
      -- * Observations
    , elements
    , head
    , contains
    , value
    , toEpisode
    
      -- * Misc
    , similarityValue
    ) where

import Data.Chronon (Chronon)
import Data.HList (HList)
import Data.Period (Period)
import Data.Set (Set, fromList, toList)
import Prelude hiding (concat, filter, head, tail)  
  
import Data.Event.EventAlgebra (Event)
import Data.Episode.EpisodeAlgebra (Episode, EpisodeAlgebra, events)

import qualified Data.Episode.EpisodeAlgebra as Episode (construct) 

---------------------------------------

-- | Internal
class InternalElement e
instance InternalElement (Event l t)
instance InternalElement (Episode a e)

-- | SimilarityValue DataType
newtype SimilarityValue = SimilarityValue Double

similarityValue :: Double -> Maybe SimilarityValue
similarityValue x = 
    if x >= 0 && x <= 1  
      then Just (SimilarityValue x)
    else Nothing
  
-- | Algebra
class (Functor Trajectory, InternalElement e, Ord e, Semigroup (Trajectory e)) => TrajectoryAlgebra e where
    data Trajectory e
    
    -- | Constructors
    construct :: Set e -> Trajectory e
    
    fromEpisode :: EpisodeAlgebra a e => Episode a e -> Trajectory e
    fromEpisode = construct . fromList .events 
        
    before, after :: Chronon t => Trajectory e -> t -> Maybe (Trajectory e)
    during :: Trajectory e -> Period c t -> Maybe (Trajectory e)
    tail :: Trajectory e -> Maybe (Trajectory e)
    append, prepend :: Trajectory e -> Trajectory e -> Trajectory e
    filter :: (e -> Bool) -> Trajectory e -> Trajectory e
     
    -- | Observations
    elements :: Trajectory e -> Set e
    head :: Trajectory e -> Maybe e
    contains :: Trajectory e -> e -> Bool
    value :: Chronon t => t -> Trajectory e -> Maybe e
    
    toEpisode :: EpisodeAlgebra a e => HList a -> Trajectory e -> Episode a e
    toEpisode a t = Episode.construct a (toList $ elements t)
    
    similarity :: Trajectory e -> Trajectory e -> SimilarityValue
    
