{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C4.T3areas
    (

    ) where
import Data.Ratio

-- data Quadro = L Int Int
data Figure a = FIG a
data Qu a = L a
newtype Tri a = TR { getTri ∷ Qu a}
newtype Cycle a = CY { getFig ∷ Figure a}
data Par a = PAR a a
class S a where
    area ∷ a → Rational

{- Почему то работает:
class F a where
    area ∷ (Num b) ⇒ a → b
instance (Integral с) ⇒ F (Qu с) where
    area (L с)= fromIntegral с

monochrom: I think you have a misunderstanding that's more basic than classes and instances.
hexagoxel: It is very well possible that a/=b even when your context provides (Num a, Num b)
monochrom: if you declare a function to have type "s :: (F a, Num b) => a -> b", it means the user, not you the author, dictates what b will be.
monochrom: whereas your coding implies that you think you the author has that right.
monochrom: your problem can be illustrate without classes. suppose you are coding up "f :: Int -> b". you have no right to say "I set b = Bool". you have no right to implement as "f x = even x". it is not up to yu.
(02:04:32) monochrom: similarly "I choose b=Int, f x = x+2"
(02:05:25) monochrom: adding classes and instances can only make this problem worse.
    -}

-- instance F (Quadro) where
--    area (L a b)= a*b
-- (Num b) ⇒
instance (Real b) ⇒ S (Qu b) where
    area (L v) = toRational $ v * v

instance (Real b) ⇒ S (Tri b) where
    area (TR (L v))= toRational $ v * v
{-
instance (Integral a) ⇒ F (Par a) where
    area (PAR a b)= fromIntegral $ a*b

instance (Integral a) ⇒ F (Figure a) where
    area (FIG a)= round $ pi* a *a
    
instance F (Quadros Int) where
    area (LM v) = v*v
-}
