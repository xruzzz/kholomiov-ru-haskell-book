{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C4_Stream
    (
        constStream,
        Stream(..),
        smap,
        zipWithS
    ) where
import Number.Nat
import qualified Prelude as P
import Prelude
infixr 5 :&
data Stream a = a :& Stream a
{-    deriving Show-}

instance Show a => Show (Stream a) where
    show xs =  showInfinity (show (stake 5 xs))
        where showInfinity x = P.init x  P.++ "..."
              
nats :: Nat -> Stream Nat
nats a = a :& nats (Succ a)

constStream :: a -> Stream a
constStream a = a :& constStream a

-- Первый элемент потока
shead :: Stream a -> a
shead (x :& _ ) = x

-- Хвост потока, всё кроме первого элемента
stail :: Stream a -> Stream a
stail (_ :& xs ) = xs
-- n-тый элемент потока
(-!!) :: Stream a -> Int -> a
(-!!) (x :& _ ) 0 = x
(-!!) (x :& xs ) n = (-!!) xs (n-1)
-- Берёт из потока несколько первых элементов:
stake :: Int -> Stream a -> [a]
stake n xxs@(x :& xs ) = htake n [] xxs
    where
        htake 0 acc _ = acc
        htake n acc (x :& xs ) = htake (n-1) (acc ++ [x]) xs

-- Преобразование потока
smap :: (a -> b) -> Stream a -> Stream b
smap f (x :& xs ) = (f x) :& smap f xs

-- Фильтрация потока
sfilter :: (a -> Bool) -> Stream a -> Stream a
sfilter f (x :& xs ) = if (f x) then (x :& sfilter f xs) else (sfilter f xs)

zip :: Stream a -> Stream b -> Stream (a, b)
zip (x :& xs ) (y :& ys ) = (x, y) :& Chapters.C4_Stream.zip xs ys

zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c 
zipWithS f (x :& xs ) (y :& ys ) = (f x y) :& (Chapters.C4_Stream.zipWithS f xs ys)

iterate :: (a -> a) -> a -> Stream a
iterate f x = f x :& Chapters.C4_Stream.iterate f (f x)
