{-# LANGUAGE UnicodeSyntax #-}
module Chapters.TypeClasses
    (

    ) where
import Control.Monad

data Qu a = L a
    deriving (Show, Eq, Ord)

instance Functor Qu where    
    fmap f (L v) = L (f v)

instance Applicative Qu where
    pure = L
    (L f) <*> dd = fmap f dd

instance Monad Qu where
    (L v) >>= fc = fc v 
    return k = L k

-- test ∷ Qu Int
-- test = do
