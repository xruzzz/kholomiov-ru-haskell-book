{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C2 where
import Number.Nat (Nat)

instance Num Nat where
    (+) a Zero     = a
    (+) a (Succ b) = Succ (a + b)
    (*) a Zero     = Zero
    (*) a (Succ b) = a + (a * b)   

    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n-1))

    abs    x    = x 
    signum Zero = Zero
    signum _    = Succ Zero

    negate _ = error "negate is undefined for Nat"

-- 1
beside1 :: Nat → Nat → Bool
beside1 x1 x2 = x1 == Succ x2 || x2 == Succ x1

-- 2
beside2 :: Nat → Nat → Bool
beside2 x1 x2 = x1 == Succ (Succ x2) || x2 == Succ (Succ x1)
{-
--3
-- 4
pow :: Nat → Nat → Nat
pow ch Zero q
pow ch p = ch * pow ch p
-}