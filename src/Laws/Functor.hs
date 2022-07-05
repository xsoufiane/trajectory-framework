{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws.Functor (Constraints, laws) where

import Test.QuickCheck hiding ((===))
import Text.Show.Functions ()

---------------------------------------------------

-- | Constraints
type Constraints f x y z =
     (
       Functor f
     , Arbitrary (f x)
     , Arbitrary x, Arbitrary y, Arbitrary z
     , CoArbitrary x, CoArbitrary y
     , Eq (f x), Eq (f z)
     , Show (f x)
     )

-- | Properties
prop_identity :: forall f x y z. Constraints f x y z => Property
prop_identity = property (\(fx :: f x) -> fmap id id fx == fx)

prop_composition :: forall f x y z. Constraints f x y z => Property
prop_composition = property (\(h :: x -> y) (g :: y -> z) (fx :: f x) -> fmap (g . h) fx == ((fmap g) . (fmap h)) fx)

-- | Laws
laws :: forall f x y z. Constraints f x y z => [(String, Property)]
laws = [ ("Identity", prop_identity @f @x @y @z), ("Composition", prop_composition @f @x @y @z) ]
