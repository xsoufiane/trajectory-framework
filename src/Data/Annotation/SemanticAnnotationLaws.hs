{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Annotation.SemanticAnnotationLaws (bifunctorLaws, laws) where

import Test.QuickCheck hiding ((===))

import Data.Annotation.SemanticAnnotationAlgebra

import qualified Laws.Bifunctor as Bifunctor

----------------------------------------------------------

-- | Laws
bifunctorLaws :: forall x x' y y' z z'.  Bifunctor.Constraints SemanticAnnotation x x' y y' z z' => [(String, Property)]
bifunctorLaws = Bifunctor.laws @SemanticAnnotation @x @x' @y @y' @z @z'

-- | Laws
laws :: forall x x' y y' z z'. Bifunctor.Constraints SemanticAnnotation x x' y y' z z' => [(String, Property)]
laws = bifunctorLaws @x @x' @y @y' @z @z'
