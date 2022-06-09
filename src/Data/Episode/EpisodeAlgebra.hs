{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Episode.EpisodeAlgebra
    (
      -- * Types
      Episode
    , EpisodeAlgebra

      -- * Constructors
    , construct
    , before
    , after
    , during
    , tail
    , mapAnnotations
    , append
    , prepend
    , filter
    
      -- * Observations
    , events
    , annotations
    , head
    , contains
    , value
    ) where

import Data.Chronon (Chronon)
import Data.HList (HList)
import Data.Kind (Type)
import Data.Period (Period)
import Prelude hiding (filter, head, tail)
import Relation.Identity (Identity)

import Data.Episode.Internal (HAnnotation)
import Data.Event.EventAlgebra (Event)

-------------------------------------------------------------------------------------------------------

-- | Internal
class InternalEvent e
instance InternalEvent (Event l t)

-- | Algebra
class 
    (
      Functor (Episode a)
    , HAnnotation a
    , Identity (Episode a e)
    , InternalEvent e
    , Semigroup (Episode a e)
    ) => EpisodeAlgebra (a :: [Type]) e 
  where
    data Episode a e
    
    -- | Constructors
    construct :: HList a -> [e] -> Episode a e
    before, after :: Chronon t => Episode a e -> t -> Maybe (Episode a e)
    during :: Episode a e -> Period c t -> Maybe (Episode a e)
    tail :: Episode a e -> Maybe (Episode a e)
    append, prepend :: Episode a e -> Episode a e -> Episode a e
    filter :: (e -> Bool) -> Episode a e -> Episode a e
    
    -- | Annotation related constructors
    mapAnnotations ::  EpisodeAlgebra a' e => (HList a -> HList a') -> Episode a e -> Episode a' e
    mapAnnotations f e = construct (f $ annotations e) (events e)

    -- | Observations
    events :: Episode a e -> [e]
    annotations :: Episode a e -> HList a
    head :: Episode a e -> Maybe e
    contains :: Episode a e -> e -> Bool
    value :: Chronon t => t -> Episode a e -> Maybe e
