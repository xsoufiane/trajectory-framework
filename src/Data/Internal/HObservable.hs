{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Internal.HObservable (HObservable) where

import Data.Kind (Type)

import Data.Observable (Observable)
  
--------------------------------------------------------------------------  
 
class HObservable (l :: [Type])
instance (Observable o, HObservable l) => HObservable (o ': l)
instance HObservable '[]
