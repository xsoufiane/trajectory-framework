{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws.Monad (Constraints, laws) where

import Control.Monad
import Test.QuickCheck hiding ((===))
import Text.Show.Functions ()

---------------------------------------------------

-- | Constraints
type Constraints m x y z w =
     (
       Monad m
     , Arbitrary (m y), Arbitrary (m z), Arbitrary (m w)
     , Arbitrary x
     , CoArbitrary x, CoArbitrary y, CoArbitrary z
     , Eq (m y), Eq (m w)
     , Show x
     )

-- | Properties
prop_left_identity :: forall m x y z w. Constraints m x y z w => Property
prop_left_identity = property (\(f :: x -> m y) x -> (return >=> f) x == f x)

prop_right_identity :: forall m x y z w. Constraints m x y z w => Property
prop_right_identity = property (\(f :: x -> m y) x -> (f >=> return) x == f x)

prop_associativity :: forall m x y z w. Constraints m x y z w => Property
prop_associativity = 
    property (\(f :: x -> m y, g :: y -> m z, h :: z -> m w) x -> ((f >=> g) >=> h) x == (f >=> (g >=> h)) x)

-- | Laws
laws :: forall m x y z w. Constraints m x y z w => [(String, Property)]
laws =
    [
      ("Left Identity", prop_left_identity @m @x @y @z @w)
    , ("Right Identity", prop_right_identity @m @x @y @z @w)
    , ("Associativity", prop_associativity @m @x @y @z @w)
    ]


