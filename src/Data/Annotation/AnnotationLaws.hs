{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Annotation.AnnotationLaws (laws) where

import Test.QuickCheck hiding ((===))

import Data.Annotation.Annotation

import qualified Laws.Functor as Functor

----------------------------------------------------------

-- | Laws
laws :: forall x y z. Functor.Constraints Annotation x y z => [(String, Property)]
laws = Functor.laws @Annotation @x @y @z
