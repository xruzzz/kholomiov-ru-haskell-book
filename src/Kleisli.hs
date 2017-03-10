{-# LANGUAGE UnicodeSyntax #-}
module Kleisli
    (
        Kleisli(..)
    ) where

import Prelude hiding (id, (>>), (*>))

class Category cat where
    id   :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

class Kleisli m where
    idK  :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idK)

-- Экземпляр для функций

instance Category (->) where
    id      = \x -> x
    f >> g  = \x -> g (f x)
