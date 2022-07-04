{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Isomorphism.Laws (Constraints, laws) where

import Prelude hiding (return)
import Test.QuickCheck hiding ((===))

import Structure.Isomorphism

-----------------------------------------------------------------------------------------

-- | Constraints
type Constraints x y =
     (
       Isomorphism x y
     , Arbitrary x, Arbitrary y
     , Eq x, Eq y
     , Show x, Show y
     )

-- | Properties
prop_iso :: forall x y. Constraints x y => Property
prop_iso = property (\(x :: x) (y :: y)  -> (g @x @y . f) x == x && (f @x @y . g) y == y )

-- | Laws
laws :: forall x y. Constraints x y => [(String, Property)]
laws = [ ("Isomorphic", prop_iso @x @y) ]
