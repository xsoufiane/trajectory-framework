{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.Annotation.Internal (Temperature) where

import Test.Tasty.QuickCheck  
  
import Data.Annotation.Annotation

-------------------------------------------------

-- | Annotation Algebra Encoding
type Temperature = Int

instance AnnotationAlgebra a where
    data Annotation a = Annotation a deriving (Show)

instance Eq a => Eq (Annotation a) where
    (Annotation x) == (Annotation y) = x == y
    
instance Functor Annotation where
    fmap f (Annotation a) = Annotation (f a)
    
instance Arbitrary a => Arbitrary (Annotation a) where
     arbitrary = fmap Annotation arbitrary   
