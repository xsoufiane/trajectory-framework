{-# LANGUAGE TypeFamilies #-}

module Data.Annotation.Annotation 
   (
     -- * Types
     Annotation
   , AnnotationAlgebra
   ) where

import Prelude hiding ((<$>), return)  

-------------------------------------------------------   

-- | Algebra
class AnnotationAlgebra a where
    data Annotation a
