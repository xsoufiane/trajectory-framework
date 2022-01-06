{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semantic.SemanticEpisodeAlgebra
    (
      -- * Types
      SemanticEpisode
      
      -- * Observations
    , semanticAnnotation
    , events
    ) where

import Data.Event (Event)
import Data.Semantic.SemanticAnnotation (SemanticAnnotation)
import Data.Time (Time)  
  
-------------------------------------------------  

class (SemanticAnnotation s, Time t) => SemanticEpisodeAlgebra l s t where
    data SemanticEpisode l s t
    
    events :: SemanticEpisode l s t -> [Event l t]
    semanticAnnotation :: SemanticEpisode l s t -> s
