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

      -- * Observations
    , annotation
    , elements
    , semanticAnnotation
    ) where

import Data.Annotation.Annotation (Annotation)
import Data.Annotation.SemanticAnnotation (SemanticAnnotation)
import Data.Episode.EpisodeAlgebra (Episode)
import Data.Event.EventAlgebra (Event)
import Data.Event.SemanticEventAlgebra (SemanticEvent)
  
-------------------------------------------------  

class InternalEvent e
instance InternalEvent (Event l t)
instance InternalEvent (SemanticEvent s l t)

class (Annotation a, InternalEvent e, SemanticAnnotation s) => SemanticEpisodeAlgebra a e s where
    data SemanticEpisode a e s
    
    -- | Constructors
    construct :: a -> [e] -> s ->  SemanticEpisode a e s
    enrich :: e ~ Event l t => Episode a l t -> s ->  SemanticEpisode a e s

    -- | Observations
    elements :: SemanticEpisode a e s -> e
    annotation :: SemanticEpisode a e s -> a
    semanticAnnotation :: SemanticEpisode a e s -> s
