{-# LANGUAGE AllowAmbiguousTypes #-}
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

      -- * Constructors
    , construct
    
      -- * Observations
    , id
    , trajectories
    , add
    , belongs
    , remove
    ) where

import Data.HList (HList)
import Data.Kind (Type)
import Prelude hiding (id)

import Data.Object.Common (TrajectoryNotFound)
import Data.Object.Internal (HTrajectory, IsTrajectory)

----------------------------------------------------------------------------------------------------

-- | Algebra
class (Functor (Object t), HTrajectory t ~ 'True) => ObjectAlgebra (t :: [Type]) id where

    data Object t id

    -- | Constructors
    construct :: HTrajectory t ~ 'True => HList t -> id -> Object t id

    -- | Observations
    id :: Object t id -> id
    trajectories :: Object t id -> HList t
    add :: HList t -> Object t id -> Object t id
    belongs :: IsTrajectory tr ~ 'True => tr -> Object t id -> Bool
    remove :: IsTrajectory tr ~ 'True => tr -> Object t id -> Either (Object t id) TrajectoryNotFound
