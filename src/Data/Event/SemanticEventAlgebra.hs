{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Event.SemanticEventAlgebra
    (
      -- * Types
      SemanticEvent
    , SemanticEventAlgebra  

      -- * Constructors
    , enrich
    , construct
    , mapObservable
    , mapAnnotations

      -- * Observations
    , observables
    , semanticAnnotations
    , time
    ) where

import Data.HList
import Data.Kind (Type)
import Data.Time (Time)
import Prelude hiding ((<))
import Structure.Identity (Identity)

import Data.Event.EventAlgebra (Event)
import Data.Internal (HObservable, HSemanticAnnotation, NotEmpty)

---------------------------------------------------------------------------------------

-- | Algebra
class 
    ( 
      Functor (SemanticEvent l s)
    , HObservable l
    , HSemanticAnnotation s
    , Identity (SemanticEvent l s t)
    , Time t
    , Semigroup (Event l t)
    , NotEmpty s ~ 'True
    ) => SemanticEventAlgebra (l :: [Type])  (s :: [Type]) t 
  where
    data SemanticEvent l s t

    -- | Constructors
    enrich :: HList s -> Event l t -> SemanticEvent l s t
    construct :: HList l -> HList s  -> t -> SemanticEvent l s t

    -- | Observable related constructors
    mapObservable 
        ::  SemanticEventAlgebra l' s t 
        => (HList l -> HList l') -> SemanticEvent l s t -> SemanticEvent l' s t
    mapObservable f e = construct (f $ observables e) (semanticAnnotations e) (time e)

    -- | Annotation related constructors
    mapAnnotations 
        ::  SemanticEventAlgebra l s' t 
        => (HList s -> HList s') -> SemanticEvent l s t -> SemanticEvent l s' t
    mapAnnotations f e = construct (observables e) (f $ semanticAnnotations e)  (time e)

    -- | Observations
    observables :: SemanticEvent l s t -> HList l
    semanticAnnotations :: SemanticEvent l s t -> HList s
    time :: SemanticEvent l s t -> t
