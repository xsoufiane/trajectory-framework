{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Event.Internal where
  
import Data.Kind (Type)

import Data.Observable (Observable)
  
------------------------------------------------------------------------------

-- | Internal
class HObservable (l :: [Type])
instance (Observable o, HObservable l) => HObservable (o ': l)
instance HObservable '[]
