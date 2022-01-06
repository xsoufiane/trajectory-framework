{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TrajectoryAlgebra (TrajectoryAlgebra (..)) where

import Data.Event (Event)
import Data.Episode (Episode)

---------------------------------------

class InternalElement e
instance InternalElement (Event l t)
instance InternalElement (Episode a l t)

class InternalElement e => TrajectoryAlgebra e where
    data Trajectory e
    
    elements :: Trajectory e -> [e]
