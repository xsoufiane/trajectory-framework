{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trajectory.Matrix
    (
      -- * Types
      RowIndex
    , ColIndex
    , Matrix
    , MatrixStruct

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
data RowIndex
data ColIndex

-- | Structure
class MatrixStruct t n  where
    data Entry n
    data Matrix n
    
    -- | Constructors
    entry :: n -> RowIndex -> ColIndex -> Entry n
    matrix :: [Entry n] -> Matrix n
    construct :: t -> Matrix n
    
    -- | Observations
    index :: Entry n -> (RowIndex, ColIndex)
    entries :: Matrix n -> [Entry n]
