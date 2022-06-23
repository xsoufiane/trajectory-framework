{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Annotation.AnnotationLaws (laws) where

import Test.QuickCheck hiding ((===))

import Data.Annotation.Annotation

import qualified Laws.Functor as Functor
import qualified Laws.Monad as Monad

----------------------------------------------------------

-- | Laws
laws :: forall x y z w. 
    (
      Functor.Constraints Annotation x y z
    , Monad.Constraints Annotation x y z w
    ) => [(String, Property)]
laws = Functor.laws @Annotation @x @y @z ++ Monad.laws @Annotation @x @y @z @w
