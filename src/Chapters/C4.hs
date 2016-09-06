{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C4
    (
    predd
    ) where

import Number.Nat (Nat(..), ℕ(..))

predd ∷ Nat → Nat
predd x = y
  where (Succ y) = x
-- 4.2 композиционный стиль
-- функции из 1 главы
not ∷ Bool → Bool
not x = if x == True then False else True

and1,and2,xor1 ∷ Bool → Bool → Bool
and1 x y = case (x, y) of
               (False,_) → False
               (True,x) → x
and2 x y = if x == False then False else y

-- xor a b = or (and (not a) b) (and a (not b))
xor1 a b = if a == b then False else True

ifThenElse :: Bool -> a -> a -> a
-- ifThenElse True   t  _ = t
-- ifThenElse False  _  e = e
ifThenElse = (\p x y → if p then x else y)

-- 4.3 инверсия стиля
-- функции из 2 главы
gets a
  | 0 <= a && a <= 3      = "Bye"
  | otherwise           = "Hello"

addZero = (\x → case x of
                       (a:[]) → '0' : a : []
                       _ → x)

fromInteger' = (\x → case x of
                       0 → Zero
                       _ → Succ (fromInteger' (x-1)))
