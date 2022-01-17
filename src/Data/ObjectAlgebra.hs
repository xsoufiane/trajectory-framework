{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Data.Annotation.SemanticAnnotation (SemanticAnnotation)
import Data.Trajectory.TrajectoryAlgebra (Trajectory)
import Data.Trajectory.SemanticTrajectoryAlgebra (SemanticTrajectory)

----------------------------------------------------------------------------------------------------

-- | Internal
type family InternalTrajectoryType (l :: [Type]) :: Bool where
    InternalTrajectoryType '[] = 'True
    InternalTrajectoryType (Trajectory e ': t) = InternalTrajectoryType t
    InternalTrajectoryType (SemanticTrajectory e s ': t) = InternalTrajectoryType t

-- | Algebra  
class (Functor (Object o), SemanticAnnotation s) => ObjectAlgebra o s where
    data Object o s
    
    -- | Constructors
    construct :: o -> s -> Object o s

    -- | Observations 
    trajectories :: InternalTrajectoryType l ~ 'True => Object o s -> HList l
    semanticAnnotation :: Object o s -> s
