{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Data.Event.SemanticEventAlgebra
    (
      -- * Types
      SemanticEvent

      -- * Constructors
    , enrich

      -- * Observations
    , observables
    , semanticAnnotation
    , time
    ) where

import Data.HList (HList)

import Data.Annotation.SemanticAnnotation (SemanticAnnotation)
import Data.Event.EventAlgebra (Event)
import Data.Internal.HObservable (HObservable)
import Data.Time (Time)

------------------------------------------------

class (Functor (SemanticEvent l t), HObservable l, SemanticAnnotation s, Time t) => SemanticEventAlgebra l s t where
    data SemanticEvent l s t

    -- | Constructors
    enrich :: s -> Event l t -> SemanticEvent l s t

    -- | Observations
    observables :: SemanticEvent l s t -> HList l
    semanticAnnotation :: SemanticEvent l s t -> s
    time :: SemanticEvent l s t -> t
