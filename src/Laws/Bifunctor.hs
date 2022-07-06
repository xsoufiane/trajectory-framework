{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws.Bifunctor (Constraints, laws) where

import Data.Bifunctor
import Test.QuickCheck hiding ((===))
import Text.Show.Functions ()

---------------------------------------------------

-- | Constraints
type Constraints p x x' y y' z z' =
     (
       Bifunctor p
     , Arbitrary (p x x')
     , Arbitrary y, Arbitrary y', Arbitrary z, Arbitrary z'
     , CoArbitrary x, CoArbitrary x', CoArbitrary y, CoArbitrary y'
     , Eq (p x x'), Eq (p z z')
     , Show (p x x')
     )

-- | Properties
prop_identity :: forall p x x' y y' z z'. Constraints p x x' y y' z z' => Property
prop_identity = property (\(pxx' :: p x x') -> bimap id id pxx' == pxx')

prop_composition :: forall p x x' y y' z z'. Constraints p x x' y y' z z' => Property
prop_composition = property (\(f :: x -> y) (f' :: x' -> y') (g :: y -> z)  (g' :: y' -> z') (pxx' :: p x x') 
    -> bimap (g . f) (g' . f') pxx' == ((bimap g g') . (bimap f f')) pxx'
    )

-- | Laws
laws :: forall p x x' y y' z z'. Constraints p x x' y y' z z' => [(String, Property)]
laws = 
    [
      ("Identity", prop_identity @p @x @x' @y @y' @z @z')
    , ("Composition", prop_composition @p @x @x' @y @y' @z @z') 
    ]
