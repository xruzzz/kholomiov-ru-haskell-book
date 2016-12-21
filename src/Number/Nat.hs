{-# LANGUAGE UnicodeSyntax #-}
module Number.Nat
    ( Nat(..),
      ℕ(..),
      VD(..),
      Number.Nat.fromInteger
    ) where

data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)

data ℕ = I' | I ℕ
    deriving (Show, Eq, Ord)

data VD a = VO | VI a
    deriving (Show, Eq, Ord)

class (Eq a, Show a) ⇒ Sum a where
  (+) ∷ a → a → a
  (-) ∷ a → a → a


class (Eq a, Show a) ⇒ Prod a where
  (*) ∷ a → a → a

class (Show a) ⇒ Conv a where
  fromInteger ∷ Integer → a

instance Sum ℕ where
  (+) a I' = I a 
  (+) a (I b) = I (a Number.Nat.+ b)
  (-) (I a) I' = a
  (-) (I a) (I b)
    | a > b = a Number.Nat.- b
    | otherwise = error $ "Первое число должно быть больше, чем второе"

instance Prod ℕ where
  (*) a I' = a
  (*) a (I b) = a Number.Nat.+ (a Number.Nat.* b)

instance Conv ℕ where
  fromInteger 1 = I'
  fromInteger n
    | n > 0 = (I $ Number.Nat.fromInteger (n Prelude.- 1))
    | otherwise = error "Должно быть больше 0"

instance Functor VD where
  fmap f VO = VO
  fmap f (VI x) = VI $ f x
