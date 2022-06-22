{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.Annotation.SemanticAnnotationSpec (spec) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Annotation.Annotation
import Data.Annotation.Context (Context)
import Data.Annotation.SemanticAnnotation
import Data.Annotation.SemanticAnnotationLaws as SemanticAnnotation (laws)
import Spec.Annotation.Internal

import Prelude hiding (return)

----------------------------------------------------------------------------------

-- | Algebra Encoding
data Weather = Warm | Cold deriving (Eq, Show)
instance Context Weather
instance Arbitrary Weather where
    arbitrary = elements [Warm, Cold]

instance SemanticAnnotationAlgebra Temperature Weather where
    data SemanticAnnotation Temperature Weather =
        SemanticAnnotation (Annotation Temperature) Weather deriving (Eq, Show)
        
    construct = SemanticAnnotation
    
    annotation (SemanticAnnotation a _) = a
    context (SemanticAnnotation _ c) = c

instance Arbitrary (SemanticAnnotation Temperature Weather) where
    arbitrary = applyArbitrary2 SemanticAnnotation

-- | spec
spec :: TestTree
spec = 
    localOption 
        (QuickCheckVerbose False)
        $ testGroup 
            "SemanticAnnotationSpec" [ semanticAnnotationLaws ]

semanticAnnotationLaws :: TestTree
semanticAnnotationLaws = testGroup "Semantic Annotation Laws"
    [
      testProperties "Semantic Annotation Laws" $ SemanticAnnotation.laws @Temperature @Weather
    ]
