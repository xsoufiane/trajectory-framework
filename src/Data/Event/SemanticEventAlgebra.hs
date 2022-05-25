{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Event.SemanticEventAlgebra
    (
      -- * Types
      SemanticEvent
    , SemanticEventAlgebra  

      -- * Constructors
    , enrich
    , construct
    , append
    , prepend
    , mapObservable
    , appendS
    , prependS
    , mapAnnotation

      -- * Observations
    , observables
    , semanticAnnotations
    , time
    ) where

import Data.HList
import Data.Kind (Type)
import Data.Time (Time)
import Prelude hiding ((<))
import Relation.Identity (Identity)

import Data.Annotation.SemanticAnnotation (SemanticAnnotation)
import Data.Event.Internal (HObservable)
import Data.Event.EventAlgebra (Event)

---------------------------------------------------------------------------------------

-- | Internal
class HSemanticAnnotation (s :: [Type])
instance HSemanticAnnotation l => HSemanticAnnotation (SemanticAnnotation c a ': l)
instance HSemanticAnnotation (SemanticAnnotation c a ': '[])

-- | Algebra
class (Functor (SemanticEvent l s), HObservable l, HSemanticAnnotation s, Identity (SemanticEvent l s t), Time t) => SemanticEventAlgebra (l :: [Type])  (s :: [Type]) t where
    data SemanticEvent l s t

    -- | Constructors
    enrich :: HList s -> Event l t -> SemanticEvent l s t
    construct :: HList l -> HList s  -> t -> SemanticEvent l s t

    -- | Observable related constructors
    append :: HObservable l' => HList l' -> SemanticEvent l s t -> SemanticEvent (HAppendListR l l') s t
    prepend :: HObservable l' => HList l' -> SemanticEvent l s t -> SemanticEvent (HAppendListR l' l) s t

    mapObservable ::  SemanticEventAlgebra l' s t => (HList l -> HList l') -> SemanticEvent l s t -> SemanticEvent l' s t
    mapObservable f e = construct (f $ observables e) (semanticAnnotations e) (time e)

    -- | Annotation related constructors
    appendS :: HSemanticAnnotation s' => HList s' -> SemanticEvent l s t -> SemanticEvent l (HAppendListR s s') t
    prependS :: HSemanticAnnotation s' => HList s' -> SemanticEvent l s t -> SemanticEvent l (HAppendListR s' s) t

    mapAnnotation ::  SemanticEventAlgebra l s' t => (HList s -> HList s') -> SemanticEvent l s t -> SemanticEvent l s' t
    mapAnnotation f e = construct (observables e) (f $ semanticAnnotations e)  (time e)

    -- | Observations
    observables :: SemanticEvent l s t -> HList l
    semanticAnnotations :: SemanticEvent l s t -> HList s
    time :: SemanticEvent l s t -> t
