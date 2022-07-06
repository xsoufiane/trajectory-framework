{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.Annotation.SemanticAnnotationSpec (spec) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Annotation.AnnotationAlgebra (AnnotationAlgebra)
import Data.Annotation.Context (Context)
import Data.Annotation.SemanticAnnotationAlgebra
import Data.Annotation.SemanticAnnotationLaws as SemanticAnnotation
import Spec.Annotation.Internal

import qualified Data.Bifunctor as Data

----------------------------------------------------------------------------------

-- | Algebra Encoding
data Weather = Warm | Cold deriving (Bounded, Enum, Eq, Show)
instance Context Weather

instance Arbitrary Weather where
    arbitrary = arbitraryBoundedEnum

instance CoArbitrary Weather where
    coarbitrary = coarbitraryEnum

instance (AnnotationAlgebra a, Context c) => SemanticAnnotationAlgebra a c where
    data SemanticAnnotation a c =
        SemanticAnnotation (Annotation a) c deriving (Eq, Show)
        
    construct = SemanticAnnotation
    
    annotation (SemanticAnnotation a _) = a
    context (SemanticAnnotation _ c) = c

instance Data.Bifunctor SemanticAnnotation where
    bimap f g (SemanticAnnotation (Annotation a) c) = SemanticAnnotation (Annotation $ f a) (g c)

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
      testProperties "Bifunctor laws"
            $ SemanticAnnotation.bifunctorLaws @Temperature @Weather @Temperature @Weather @Temperature @Weather
    ]
