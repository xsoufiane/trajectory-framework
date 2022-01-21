{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Object.ObjectAlgebra
    (
      -- * Types
      ObjectAlgebra
    , Object

      -- * Constructors
    , construct
    
      -- * Observations
    , trajectories
    ) where

import Data.HList (HList)

import Data.Internal.HTrajectory (HTrajectory)

----------------------------------------------------------------------------------------------------

-- | Algebra  
class ObjectAlgebra o where
    data Object o
    
    -- | Constructors
    construct :: o -> Object o

    -- | Observations 
    trajectories :: HTrajectory l ~ 'True => Object o -> HList l
