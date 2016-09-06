import Data.Ratio

data Qu a = L a
newtype Tri a = TR { getTri ∷ Qu a}
class S a where
    square ∷ a → Rational
-- It's ok:
instance (Real b) ⇒ S (Qu b) where
    square (L v) = toRational $ v * v
-- It's trouble:
instance (Real b) ⇒ S (Tri b) where
    square (TR v)= toRational $ v * v

{-
Errors:
Chapters/C4_3squares.hs:39:20:
    Could not deduce (Real (Qu b)) arising from a use of ‘toRational’
    from the context (Real b)
      bound by the instance declaration
      at Chapters/C4_3squares.hs:38:10-29
    In the expression: toRational
    In the expression: toRational $ v * v
    In an equation for ‘square’: square (TR v) = toRational $ v * v

Chapters/C4_3squares.hs:39:35:
    Could not deduce (Num (Qu b)) arising from a use of ‘*’
    from the context (Real b)
      bound by the instance declaration
      at Chapters/C4_3squares.hs:38:10-29
    In the second argument of ‘($)’, namely ‘v * v’
    In the expression: toRational $ v * v
    In an equation for ‘square’: square (TR v) = toRational $ v * v

        -}
