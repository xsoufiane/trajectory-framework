{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.Annotation.Internal where

import Data.Annotation.AnnotationAlgebra
import Prelude hiding ((<$>), return)
import Test.Tasty.QuickCheck
import Structure.Isomorphism

import qualified Control.Comonad as Comonad

-------------------------------------------------

-- | Annotation Algebra Encoding
type Temperature = Int

instance AnnotationAlgebra a where
    data Annotation a = Annotation a deriving (Show)
    
    -- | Constructors
    f <$> Annotation a = return $ f a
    return = Annotation
    f >=> g = g . extract . f
    
    -- | Observations
    extract (Annotation a) = a
    f =>= g = g . return . f

-- | Spec Instances
instance Eq a => Eq (Annotation a) where
    (Annotation x) == Annotation y = x == y    
    
instance Functor Annotation where
    fmap f (Annotation a) = Annotation (f a)
   
instance Applicative Annotation where
     pure = return
     (Annotation f) <*> Annotation a = pure $ f a
     
instance Monad Annotation where
    (Annotation a) >>= f = f a
    
instance Comonad.Comonad Annotation where
    extract (Annotation a) = a
    duplicate = return
    
instance Isomorphism a (Annotation a) where
    f = return
    g = extract
    
instance Arbitrary a => Arbitrary (Annotation a) where
     arbitrary = fmap Annotation arbitrary

instance CoArbitrary a => CoArbitrary (Annotation a) where
     coarbitrary = coarbitrary . extract 
