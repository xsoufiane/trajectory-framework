{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trajectory.Tensor
    (
      -- * Types
      XIndex
    , YIndex
    , ZIndex
    , Tensor
    , TensorStruct

      -- * Constructors
    , entry
    , matrix
    , construct

      -- * Observations
    , index
    , entries
    ) where

-----------------------------------------------------------

-- | Indices
data XIndex
data YIndex
data ZIndex

-- | Structure
class TensorStruct t n  where
    data Entry n
    data Tensor n

    -- | Constructors
    entry :: n -> XIndex -> YIndex -> ZIndex -> Entry n
    matrix :: [Entry n] -> Tensor n
    construct :: t -> Tensor n

    -- | Observations
    index :: Entry n -> (XIndex, YIndex, ZIndex)
    entries :: Tensor n -> [Entry n]
