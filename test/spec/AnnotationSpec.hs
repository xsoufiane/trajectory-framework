{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AnnotationSpec (spec) where

import Data.Annotation.Laws (laws)
import Relation.Identity (Identity((===)))
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((===))

import Data.Annotation.Annotation
import Data.Annotation.SemanticAnnotation
import Data.Annotation.Context (Context)

------------------------------------

-- | Algebra Encoding
-- | Annotation
type Temperature = Int
instance AnnotationAlgebra Temperature where
    data Annotation Temperature = Annotation Temperature deriving (Show)
    
    construct = Annotation

instance Identity (Annotation Temperature) where
    (Annotation x) === (Annotation y) = x == y
    
instance Arbitrary (Annotation Temperature) where
    arbitrary = Annotation <$> arbitrary

-- | Semantic Annotation
data Weather = Warm | Cold deriving (Eq, Show)
instance Context Weather
instance Arbitrary Weather where
    arbitrary = elements [Warm, Cold]

instance SemanticAnnotationAlgebra Temperature Weather where
    data SemanticAnnotation Temperature Weather = 
        SemanticAnnotation (Annotation Temperature) Weather deriving (Show)
    
    construct a c = SemanticAnnotation a c
    annotation (SemanticAnnotation a _) = a
    context (SemanticAnnotation _ w) = w

instance Identity (SemanticAnnotation Temperature Weather) where
    (SemanticAnnotation (Annotation x) y) === (SemanticAnnotation (Annotation x') y') = x == x' && y == y'

instance Eq (SemanticAnnotation Temperature Weather) where
    x == y = x === y   

instance Arbitrary (SemanticAnnotation Temperature Weather) where
    arbitrary = applyArbitrary2 SemanticAnnotation
    
-- | spec
spec :: TestTree
spec = testGroup "AnnotationSpec" [ semanticAnnotationLaws ]

semanticAnnotationLaws :: TestTree
semanticAnnotationLaws = testGroup "Semantic Annotation Laws"
    [
      testProperties "Semantic Annotation Laws" $ laws @Temperature @Weather
    ]
