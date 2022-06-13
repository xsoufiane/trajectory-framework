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
   
     -- * Observations
   , annotation
   , context
   ) where

import Relation.Identity (Identity)

import Data.Annotation.Annotation (Annotation, AnnotationAlgebra)  
import Data.Annotation.Context (Context)

-------------------------------------------------------     

class (AnnotationAlgebra a, Context c, Identity (SemanticAnnotation a c)) => SemanticAnnotationAlgebra a c where
    data SemanticAnnotation a c
    
    -- | Constructors
    construct :: Annotation a -> c -> SemanticAnnotation a c
    
    -- | Observations 
    annotation :: SemanticAnnotation a c -> a
    context :: SemanticAnnotation a c -> c
