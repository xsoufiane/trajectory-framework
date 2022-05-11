{-# LANGUAGE TypeFamilies #-}

module Data.Annotation.Annotation 
   (
     -- * Types
     Annotation
   , AnnotationAlgebra
   
     -- * Constructors
   , construct
   ) where

-------------------------------------------------------   
  
class AnnotationAlgebra a where
    data Annotation a
    
    -- | Constructors
    construct :: a -> Annotation a
