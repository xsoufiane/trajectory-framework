{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Object.ObjectAlgebra
    (
      -- * Types
      ObjectAlgebra
    , Object
    , Id

      -- * Constructors
    , construct
    
      -- * Observations
    , id
    , trajectories
    , add
    , belongs
    , remove
    ) where

import Data.Kind (Type)  
import Data.HList (HList)
import Prelude hiding (id)

import Data.Trajectory.TrajectoryAlgebra (Trajectory)
import Data.Trajectory.SemanticTrajectoryAlgebra (SemanticTrajectory)

----------------------------------------------------------------------------------------------------

-- | Internal
type family HTrajectory (l :: [Type]) :: Bool where
    HTrajectory '[] = 'True
    HTrajectory (Trajectory e ': t) = HTrajectory t
    HTrajectory (SemanticTrajectory e s ': t) = HTrajectory t

-- | Exceptions


-- | Algebra
data Result = Success | Failure

data Id
data Object

class ObjectAlgebra obj where
  
    -- | Constructors
    construct :: HTrajectory l ~ 'True => HList l -> Id -> obj

    -- | Observations
    id :: obj -> Id
    trajectories :: HTrajectory l ~ 'True => obj -> HList l
    add :: HTrajectory l ~ 'True => HList l -> obj -> Result
    belongs :: HTrajectory l ~ 'True => HList l -> obj -> Result
    remove :: HTrajectory l ~ 'True => HList l -> obj -> Result
