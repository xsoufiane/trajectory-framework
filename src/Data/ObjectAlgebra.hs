{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ObjectAlgebra
    (
      -- * Types
      ObjectAlgebra
    , Object

      -- * Constructors
    , construct
    
      -- * Observations
    , trajectories
    , semanticAnnotation
    ) where

import Data.HList (HList)
import Data.Kind (Type)

import Data.TrajectoryAlgebra (Trajectory)
import Data.Semantic.SemanticAnnotation (SemanticAnnotation)

-- | Internal
class HTrajectory (l :: [Type])
instance HTrajectory l => HTrajectory (Trajectory e s ': l)
instance HTrajectory '[]

-- | Algebra  
class SemanticAnnotation s => ObjectAlgebra o s where
    data Object o s
    
    -- | Constructors
    construct :: o -> s -> Object o s

    -- | Observations 
    trajectories :: HTrajectory l => Object o s -> HList l
    semanticAnnotation :: Object o s -> s
