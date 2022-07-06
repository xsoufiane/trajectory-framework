{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Annotation.AnnotationLaws (functorLaws, monadLaws, comonadLaws, isomorphismLaws, laws) where

import Prelude hiding (return)
import Test.QuickCheck hiding ((===))

import Data.Annotation.AnnotationAlgebra

import qualified Laws.Functor as Functor
import qualified Laws.Monad as Monad
import qualified Laws.Comonad as Comonad
import qualified Structure.Isomorphism.Laws as Isomorphism

----------------------------------------------------------

-- | Laws
functorLaws :: forall x y z.  Functor.Constraints Annotation x y z => [(String, Property)]
functorLaws = Functor.laws @Annotation @x @y @z

monadLaws :: forall x y z w. Monad.Constraints Annotation x y z w => [(String, Property)]
monadLaws = Monad.laws @Annotation @x @y @z @w

comonadLaws :: forall x y z w. Comonad.Constraints Annotation x y z w => [(String, Property)]
comonadLaws = Comonad.laws @Annotation @x @y @z @w

isomorphismLaws :: forall x. Isomorphism.Constraints x (Annotation x) => [(String, Property)]
isomorphismLaws = Isomorphism.laws @x @(Annotation x)

-- | All the laws
laws :: forall x y z w. 
    (
      Functor.Constraints Annotation x y z
    , Monad.Constraints Annotation x y z w
    , Comonad.Constraints Annotation x y z w
    , Isomorphism.Constraints x (Annotation x)
    ) => [(String, Property)]
laws = functorLaws @x @y @z
     ++ monadLaws @x @y @z @w
     ++ comonadLaws @x @y @z @w
     ++ isomorphismLaws @x
