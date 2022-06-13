{-# LANGUAGE FunctionalDependencies #-}

module Data.TrajectoryLike
    (
      -- * Type
      TrajectoryLike

      -- * Constructors
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
    , similarity
    , estimate
    , clusters
    , anomalies
    , outliers
    
      -- * Misc
    , SimilarityValue
    , similarityValue
    ) where

import Data.Chronon (Chronon)
import Data.Period (Period)
import Data.Time (Time)
import Prelude hiding (filter, head, tail)
  
---------------------------------------------------------------------

-- | SimilarityValue DataType
newtype SimilarityValue = SimilarityValue Double

similarityValue :: Double -> Maybe SimilarityValue
similarityValue x = 
    if x >= 0 && x <= 1  
      then Just (SimilarityValue x)
    else Nothing

-- | Trajectory Structure
class TrajectoryLike t e | t -> e where
    -- | Constructors
    before, after :: Chronon t => t -> t -> Maybe t
    during :: t -> Period c t -> Maybe t
    tail :: t -> Maybe t
    append, prepend :: t -> t -> t
    filter :: (e -> Bool) -> t -> t
    
    -- | Observations
    elements :: t -> [e]
    head :: t -> Maybe e
    contains :: t -> e -> Bool
    value :: Chronon x => x -> t -> Maybe e
    similarity :: t -> t -> SimilarityValue
    estimate :: Time z => (z -> t -> e) -> z -> t -> e
    clusters :: [t] -> [[t]]
    anomalies :: (e -> Bool) -> t -> [e]
    outliers :: (t -> Bool) -> [t] -> [t]
