{-# LANGUAGE MultiParamTypeClasses #-}

module Structure.Isomorphism (Isomorphism(f, g)) where

---------------------------------------------------
class Isomorphism a b where
    f :: a -> b
    g :: b -> a
