{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.Annotation.AnnotationSpec (spec) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Spec.Annotation.Internal

import qualified Data.Annotation.AnnotationLaws as AnnotationLaws

----------------------------------------------------------------------------------

-- | spec
spec :: TestTree
spec = 
    localOption 
        (QuickCheckVerbose False)
        $ testGroup 
            "AnnotationSpec" [ annotationLaws ]

annotationLaws :: TestTree
annotationLaws = testGroup "Annotation Laws"
    [
      testProperties "Functor" $ AnnotationLaws.functorLaws @Temperature @Temperature @Temperature
    , testProperties "Monad" $ AnnotationLaws.monadLaws @Temperature @Temperature @Temperature @Temperature
    , testProperties "Comonad" $ AnnotationLaws.comonadLaws @Temperature @Temperature @Temperature @Temperature
    , testProperties "Isomorphism" $ AnnotationLaws.isomorphismLaws @Temperature
    ]
