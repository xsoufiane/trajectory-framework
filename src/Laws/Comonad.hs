{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws.Comonad (Constraints, laws) where

import Control.Comonad
import Test.QuickCheck hiding ((===))
import Text.Show.Functions ()

---------------------------------------------------

-- | Constraints
type Constraints m x y z w =
     (
       Comonad m
     , Arbitrary y, Arbitrary z, Arbitrary w
     , Arbitrary (m x)
     , CoArbitrary (m x), CoArbitrary (m y), CoArbitrary (m z)
     , Eq y, Eq w
     , Show (m x)
     )

-- | Properties
prop_left_identity :: forall m x y z w. Constraints m x y z w => Property
prop_left_identity = property (\(f :: m x -> y) x -> (extract =>= f) x == f x)

prop_right_identity :: forall m x y z w. Constraints m x y z w => Property
prop_right_identity = property (\(f :: m x -> y) x -> (f =>= extract) x == f x)

prop_associativity :: forall m x y z w. Constraints m x y z w => Property
prop_associativity = 
    property (\(f :: m x -> y, g :: m y -> z, h :: m z -> w) x -> ((f =>= g) =>= h) x == (f =>= (g =>= h)) x)

-- | Laws
laws :: forall m x y z w. Constraints m x y z w => [(String, Property)]
laws =
    [
      ("Left Identity", prop_left_identity @m @x @y @z @w)
    , ("Right Identity", prop_right_identity @m @x @y @z @w)
    , ("Associativity", prop_associativity @m @x @y @z @w)
    ]
