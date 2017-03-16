{-# LANGUAGE UnicodeSyntax #-}
module Main where
import Number.Nat
import qualified Chapters.C4.Functions as CC4 (predd)

main ∷ IO ()
main = do
  print $ Succ(Succ Zero)
  print $ CC4.predd (Succ(Succ Zero))
