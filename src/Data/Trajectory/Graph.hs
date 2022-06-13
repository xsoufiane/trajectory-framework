{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trajectory.Graph
    (
      -- * Types
      Graph
    , Node
    , Edge
    , GraphStruct
              
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

class GraphStruct t n where
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
    