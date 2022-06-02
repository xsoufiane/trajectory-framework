{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Episode.SemanticEpisodeAlgebra
    (
      -- * Types
      SemanticEpisode
    
      -- * Constructors
    , construct
    , enrich
    , mapAnnotations
    , mapSemanticAnnotations

      -- * Observations
    , annotations
    , events
    , semanticAnnotations
    ) where

import Data.HList (HList)  
import Data.Kind (Type)

import Data.Episode.EpisodeAlgebra (Episode)
import Data.Event.EventAlgebra (Event)
import Data.Event.SemanticEventAlgebra (SemanticEvent)
import Data.Episode.Internal (HAnnotation)
import Data.Internal (HSemanticAnnotation)
  
-------------------------------------------------  

-- | Internal
class InternalEvent e
instance InternalEvent (Event l t)
instance InternalEvent (SemanticEvent s l t)

-- | Algebra
class (HAnnotation a, InternalEvent e, HSemanticAnnotation s) => SemanticEpisodeAlgebra (a :: [Type]) e (s :: [Type]) where
    data SemanticEpisode a e s
    
    -- | Constructors
    construct :: HList a -> [e] -> HList s ->  SemanticEpisode a e s
    enrich :: e ~ Event l t => Episode a e -> HList s ->  SemanticEpisode a e s
    
    -- | Annotation related constructors
    mapAnnotations ::  SemanticEpisodeAlgebra a' e s => (HList a -> HList a') -> SemanticEpisode a e s -> SemanticEpisode a' e s
    mapAnnotations f e = construct (f $ annotations e) (events e) (semanticAnnotations e)
    
    -- | Semantic Annotation related constructors
    mapSemanticAnnotations ::  SemanticEpisodeAlgebra a e s' => (HList s -> HList s') -> SemanticEpisode a e s -> SemanticEpisode a e s'
    mapSemanticAnnotations f e = construct (annotations e) (events e) (f $ semanticAnnotations e)

    -- | Observations
    events :: SemanticEpisode a e s -> [e]
    annotations :: SemanticEpisode a e s -> HList a
    semanticAnnotations :: SemanticEpisode a e s -> HList s
