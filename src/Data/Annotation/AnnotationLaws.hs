{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Annotation.AnnotationLaws (functorLaws, monadLaws, comonadLaws, laws) where

import Test.QuickCheck hiding ((===))

import Data.Annotation.Annotation

import qualified Laws.Functor as Functor
import qualified Laws.Monad as Monad
import qualified Laws.Comonad as Comonad

----------------------------------------------------------

-- | Laws
functorLaws :: forall x y z.  Functor.Constraints Annotation x y z => [(String, Property)]
functorLaws = Functor.laws @Annotation @x @y @z

monadLaws :: forall x y z w. Monad.Constraints Annotation x y z w => [(String, Property)]
monadLaws = Monad.laws @Annotation @x @y @z @w

comonadLaws :: forall x y z w. Comonad.Constraints Annotation x y z w => [(String, Property)]
comonadLaws = Comonad.laws @Annotation @x @y @z @w

laws :: forall x y z w. 
    (
      Functor.Constraints Annotation x y z
    , Monad.Constraints Annotation x y z w
    , Comonad.Constraints Annotation x y z w
    ) => [(String, Property)]
laws = functorLaws @x @y @z ++ monadLaws @x @y @z @w ++ comonadLaws @x @y @z @w
