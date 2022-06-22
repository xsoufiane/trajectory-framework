{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Annotation.SemanticAnnotationLaws (laws) where

import Test.QuickCheck hiding ((===))

import Data.Annotation.SemanticAnnotation

----------------------------------------------------------

-- | Constraints
type Constraints a c =
     (
       Arbitrary (SemanticAnnotation a c)
     , Eq (SemanticAnnotation a c)
     , SemanticAnnotationAlgebra a c
     , Show (SemanticAnnotation a c)
     )

-- | Properties
prop_construct :: forall a c. Constraints a c => Property
prop_construct = property (\(x :: SemanticAnnotation a c) -> x == construct (annotation x) (context x))

-- | Laws
laws :: forall a c. Constraints a c => [(String, Property)]
laws = [ ("Construct", prop_construct @a @c) ]
