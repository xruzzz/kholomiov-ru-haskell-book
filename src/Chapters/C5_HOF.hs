{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C5_HOF where
-- Исходный вариант
sumList ∷ (Num a) => [a] -> a
sumList [x] = x
sumList (x:xs) = x + sumList xs

-- С использованием функции высшего порядка
foldList ∷ (a -> a -> a) -> [a] -> a
foldList op [x]     = x
foldList op (x:xs) = x `op` (foldList op xs)

sumList1 ∷ (Num a) => [a] -> a
sumList1 = foldList (+) 
