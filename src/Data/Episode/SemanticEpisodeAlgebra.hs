{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Relation.Identity (Identity)

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
class 
    (
      Functor (SemanticEpisode a s)
    , HAnnotation a
    , Identity (Episode a e)
    , InternalEvent e
    , HSemanticAnnotation s
    , Semigroup (SemanticEpisode a s e)
    ) => SemanticEpisodeAlgebra (a :: [Type]) (s :: [Type]) e 
  where
    data SemanticEpisode a s e
    
    -- | Constructors
    construct :: HList a -> [e] -> HList s ->  SemanticEpisode a s e
    enrich :: e ~ Event l t => Episode a e -> HList s ->  SemanticEpisode a s e

    -- | Annotation related constructors
    mapAnnotations 
        :: SemanticEpisodeAlgebra a' s e 
        => (HList a -> HList a') -> SemanticEpisode a s e -> SemanticEpisode a' s e
    mapAnnotations f e = construct (f $ annotations e) (events e) (semanticAnnotations e)

    -- | Semantic Annotation related constructors
    mapSemanticAnnotations 
        :: SemanticEpisodeAlgebra a s' e
        => (HList s -> HList s') -> SemanticEpisode a s e -> SemanticEpisode a s' e
    mapSemanticAnnotations f e = construct (annotations e) (events e) (f $ semanticAnnotations e)

    -- | Observations
    events :: SemanticEpisode a s e -> [e]
    annotations :: SemanticEpisode a s e -> HList a
    semanticAnnotations :: SemanticEpisode a s e -> HList s
