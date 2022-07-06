{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Episode.Internal (HAnnotation) where
  
import Data.Kind (Type) 

import Data.Annotation.AnnotationAlgebra (Annotation)

-----------------------------------------------------------------

-- | Internal
class HAnnotation (a :: [Type])
instance HAnnotation l => HAnnotation (Annotation a ': l)
instance HAnnotation (Annotation a ': '[])


