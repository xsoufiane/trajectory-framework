{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Annotation.Laws (laws) where

import Test.QuickCheck hiding ((===))

import Data.Annotation.SemanticAnnotation
  
----------------------------------------------------------

type Constraints a c = 
     (
       Arbitrary (SemanticAnnotation a c)
     , Eq (SemanticAnnotation a c)
     , SemanticAnnotationAlgebra a c
     , Show (SemanticAnnotation a c)
     )

prop_construct :: forall a c. Constraints a c => Property
prop_construct = property (\(x :: SemanticAnnotation a c) -> x == construct (annotation x) (context x))

laws :: forall a c. Constraints a c => [(String, Property)]
laws = [ ("Construct", prop_construct @a @c) ]
