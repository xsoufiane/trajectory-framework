{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Object.SemanticObjectAlgebra
    (
      -- * Types
      SemanticObjectAlgebra
    , SemanticObject

      -- * Constructors
    , construct
    , enrich
    , mapAnnotations
    
      -- * Observations
    , id
    , trajectories
    , semanticAnnotations
    , add
    , belongs
    , remove
    ) where

import Data.HList (HList)
import Data.Kind (Type)
import Prelude hiding (id)

import Data.Internal (HSemanticAnnotation, NotEmpty)
import Data.Object.Common (TrajectoryNotFound)
import Data.Object.Internal (HTrajectory, IsTrajectory)
import Data.Object.ObjectAlgebra (Object)

----------------------------------------------------------------------------------------------------

-- | Algebra  
class
    (
      Functor (SemanticObject s t)
    , HSemanticAnnotation s
    , HTrajectory t ~ 'True
    , NotEmpty s ~ 'True
    ) => SemanticObjectAlgebra (s :: [Type]) (t :: [Type]) id
  where
    data SemanticObject s t id
    
    -- | Constructors
    construct :: HList s -> id -> HList l -> SemanticObject s t id
    enrich :: Object t id -> HList s -> SemanticObject s t id

    -- | Annotation related constructors
    mapAnnotations
        ::  SemanticObjectAlgebra s' t id
        => (HList s -> HList s') -> SemanticObject s t id -> SemanticObject s' t id
    mapAnnotations f obj = construct (f $ semanticAnnotations obj) (id obj) (trajectories obj)

    -- | Observations
    id :: SemanticObject s t id -> id
    semanticAnnotations :: SemanticObject s t id -> HList s
    trajectories :: SemanticObject s t id -> HList t
    add :: HList t -> SemanticObject s t id -> SemanticObject s t id
    belongs :: IsTrajectory tr ~ 'True => tr -> SemanticObject s t id -> Bool
    remove
        :: IsTrajectory tr ~ 'True
        => tr -> SemanticObject s t id -> Either (SemanticObject s t id) TrajectoryNotFound
