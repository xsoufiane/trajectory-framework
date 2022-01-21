{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Internal.HTrajectory (HTrajectory) where

import Data.Kind (Type)

import Data.Trajectory.TrajectoryAlgebra (Trajectory)
import Data.Trajectory.SemanticTrajectoryAlgebra (SemanticTrajectory)

-------------------------------------------------------------------------------------------

type family HTrajectory (l :: [Type]) :: Bool where
    HTrajectory '[] = 'True
    HTrajectory (Trajectory e ': t) = HTrajectory t
    HTrajectory (SemanticTrajectory e s ': t) = HTrajectory t
