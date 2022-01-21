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
    
      -- * Observations
    , trajectories
    , semanticAnnotation
    ) where

import Data.HList (HList)

import Data.Annotation.SemanticAnnotation (SemanticAnnotation)
import Data.Internal.HTrajectory (HTrajectory)
import Data.Object.ObjectAlgebra (Object)

----------------------------------------------------------------------------------------------------

-- | Algebra  
class (Functor (SemanticObject o), SemanticAnnotation s) => SemanticObjectAlgebra o s where
    data SemanticObject o s
    
    -- | Constructors
    construct :: o -> s -> SemanticObject o s
    enrich :: Object o -> s -> SemanticObject o s

    -- | Observations 
    trajectories :: HTrajectory l ~ 'True => SemanticObject o s -> HList l
    semanticAnnotation :: SemanticObject o s -> s
