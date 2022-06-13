{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Annotation.Annotation 
   (
     -- * Types
     Annotation
   , AnnotationAlgebra
   
     -- * Constructors
   , construct
   ) where

import Relation.Identity (Identity)
  
-------------------------------------------------------   
  
class (Identity (Annotation a)) => AnnotationAlgebra a where
    data Annotation a
    
    -- | Constructors
    construct :: a -> Annotation a
