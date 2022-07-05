{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Annotation.SemanticAnnotation 
   (
     -- * Types
     SemanticAnnotation
   , SemanticAnnotationAlgebra
   
     -- * Constructors
   , construct
   , bimap
   
     -- * Observations
   , annotation
   , context
   ) where

import Data.Annotation.Annotation (Annotation, AnnotationAlgebra)  
import Data.Annotation.Context (Context)

-------------------------------------------------------     

class (AnnotationAlgebra a, Context c) => SemanticAnnotationAlgebra a c where
    data SemanticAnnotation a c
    
    -- | Constructors
    construct :: Annotation a -> c -> SemanticAnnotation a c
    bimap :: (a -> b) -> (c -> d) -> SemanticAnnotation a c -> SemanticAnnotation b d
     
    -- | Observations 
    annotation :: SemanticAnnotation a c -> Annotation a
    context :: SemanticAnnotation a c -> c
