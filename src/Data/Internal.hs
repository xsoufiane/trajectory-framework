{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Internal (HObservable, HSemanticAnnotation, NotEmpty) where

import Data.Kind (Type)

import Data.Annotation.SemanticAnnotationAlgebra (SemanticAnnotation)
import Data.Observable (Observable)

--------------------------------------------------

-- | Internal
class HObservable (l :: [Type])
instance (Observable o, HObservable l) => HObservable (o ': l)
instance HObservable '[]

class HSemanticAnnotation (s :: [Type])
instance HSemanticAnnotation l => HSemanticAnnotation (SemanticAnnotation c a ': l)
instance HSemanticAnnotation (SemanticAnnotation c a ': '[])

type family NotEmpty (a :: [Type]) :: Bool where
    NotEmpty (x ': xs) = 'True
    NotEmpty _ = 'False
