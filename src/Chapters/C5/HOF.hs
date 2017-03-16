{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C5.HOF where
-- 5.1.1
-- Исходный вариант
sumList ∷ (Num a) => [a] -> a
sumList [x] = x
sumList (x:xs) = x + sumList xs

-- С использованием функции высшего порядка
foldR1List ∷ (a -> a -> a) -> [a] -> a
foldR1List op [x]     = x
foldR1List op (x:xs) = x `op` (foldR1List op xs)

sumList1 ∷ (Num a) => [a] -> a
sumList1 = foldR1List (+) 

-- 5.1.2
-- 99 questions - 2

{-
    Общее правило для функций обрабатывающих списки
        - Применить ФВП в бесточечном стиле

    -}

foldL1 ∷ (a -> a -> a) -> [a] -> a
foldL1 op [] = error "nooo"
foldL1 op [x] = error "nooo"
foldL1 op [x,y] = x `op` y
foldL1 op xs = op (foldL1 op (init xs)) $ last xs

scanL1List ∷ (a -> a -> a) -> [a] -> [a]
scanL1List op []    = []
scanL1List op [x]   = []
scanL1List op [x,y] = [x `op` y]
scanL1List op xs =  scanL1List op (init xs) ++ [foldL1 op xs]
