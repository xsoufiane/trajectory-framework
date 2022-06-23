{-# LANGUAGE TypeFamilies #-}

module Data.Annotation.Annotation 
   (
     -- * Types
     Annotation
   , AnnotationAlgebra

     -- * Constructors
   , (<$>)
   , return
   , (>=>)
   
     -- * Observations
   , extract
   ) where

import Prelude hiding ((<$>), return)

-------------------------------------------------------   

-- | Algebra
class AnnotationAlgebra a where
    data Annotation a

    -- | Constructors
    (<$>) :: (a -> b) -> Annotation a -> Annotation b
    return :: a -> Annotation a
    (>=>) :: (a -> Annotation b) -> (b -> Annotation c) -> (a -> Annotation c)
    
    -- | Observations
    extract :: Annotation a -> a
