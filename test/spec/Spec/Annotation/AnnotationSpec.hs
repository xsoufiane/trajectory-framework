{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.Annotation.AnnotationSpec (spec) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Annotation.Annotation
import Laws.Functor as Functor (laws) 
import Spec.Annotation.Internal

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
      testProperties "Functor Laws" $ Functor.laws @Annotation @Temperature @Temperature @Temperature
    ]
