{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semantic.SemanticEventAlgebra
    (
      -- * Types
      SemanticEvent
      
      -- * Constructors
    , annotate
    
      -- * Observations
    , semanticAnnotation
    , observables
    , time
    ) where

import Data.HList (HList)
 
import Data.Event (Event)  
import Data.Semantic.SemanticAnnotation (SemanticAnnotation)
import Data.Time (Time)  

------------------------------------------------

class (SemanticAnnotation s, Time t) => SemanticEventAlgebra l s t where
    data SemanticEvent l s t
    
    -- | Constructors
    annotate :: s -> Event l t -> SemanticEvent l s t
    
    -- | Observations
    semanticAnnotation :: SemanticEvent l s t -> s
    observables :: SemanticEvent l s t -> HList l
    time :: SemanticEvent l s t -> t
