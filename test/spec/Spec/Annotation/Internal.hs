{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.Annotation.Internal (Temperature) where

import Test.Tasty.QuickCheck  
  
import Data.Annotation.Annotation
import Prelude hiding ((<$>), return)

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

-- | Spec Instances
instance Eq y => Eq (x -> y) where
    f == g = undefined
    
instance Eq a => Eq (Annotation a) where
    (Annotation x) == Annotation y = x == y    
    
instance Functor Annotation where
    fmap f (Annotation a) = Annotation (f a)
   
instance Applicative Annotation where
     pure = return
     (Annotation f) <*> Annotation a = pure $ f a
     
instance Monad Annotation where
    (Annotation a) >>= f = f a
    
instance Arbitrary a => Arbitrary (Annotation a) where
     arbitrary = fmap Annotation arbitrary   
