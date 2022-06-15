{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Object.Internal (HTrajectory, IsTrajectory) where
  
import Data.Kind (Type) 

import Data.Trajectory.TrajectoryAlgebra (Trajectory)
import Data.Trajectory.SemanticTrajectoryAlgebra (SemanticTrajectory)

-----------------------------------------------------------------

-- | Internal
type family HTrajectory (l :: [Type]) :: Bool where
    HTrajectory '[] = 'True
    HTrajectory (Trajectory e ': t) = HTrajectory t
    HTrajectory (SemanticTrajectory e s ': t) = HTrajectory t
    
type family IsTrajectory t :: Bool where
    IsTrajectory (Trajectory _) = 'True
    IsTrajectory (SemanticTrajectory _ _) = 'True
    IsTrajectory _ = 'False
