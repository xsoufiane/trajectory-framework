{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Internal (HObservable, HSemanticAnnotation) where

import Data.Kind (Type)

import Data.Observable (Observable)
import Data.Annotation.SemanticAnnotation (SemanticAnnotation)

--------------------------------------------------

-- | Internal
class HObservable (l :: [Type])
instance (Observable o, HObservable l) => HObservable (o ': l)
instance HObservable '[]

class HSemanticAnnotation (s :: [Type])
instance HSemanticAnnotation l => HSemanticAnnotation (SemanticAnnotation c a ': l)
instance HSemanticAnnotation (SemanticAnnotation c a ': '[])
