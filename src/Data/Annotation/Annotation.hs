{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Annotation.Annotation 
   (
     -- * Types
     Annotation
   , AnnotationAlgebra
   
     -- * Constructors
   , return
   , (<$>)
   ) where

import Prelude hiding ((<$>), return)  
--import Relation.Identity (Identity)

-------------------------------------------------------   

-- | Algebra
--class Identity (Annotation a) => AnnotationAlgebra a where
class AnnotationAlgebra a where
    data Annotation a
    
    -- | Constructors
    return :: a -> Annotation a
    (<$>) :: AnnotationAlgebra b => (a -> b) -> Annotation a -> Annotation b
          
--    -- | Observations
--    extract :: Annotation a -> a


