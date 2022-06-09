{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trajectory.Graph
    (
      -- * Types
      TrajectoryGraph
    , Node
    , Edge
    , Graph
              
      -- * Constructors
    , node
    , edge
    , graph
              
      -- * Observations
    , construct
    , nodes
    , edges
    ) where

----------------------------------------

class TrajectoryGraph t n where
    data Node n
    data Edge n
    data Graph n
    
    -- | Constructors
    node :: n -> Node n
    edge :: n -> n -> Edge n
    graph :: [Node n] -> [Edge n] -> Graph n
    construct :: t -> Graph n
    
    -- | Observations
    nodes :: Graph n -> [Node n]
    edges :: Graph n -> [Edge n]
    