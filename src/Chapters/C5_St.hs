{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C5_St
    (
        St(..),
        ap
    ) where
import qualified Prelude as P
import Prelude

import qualified Chapters.C4_Stream as C4 (Stream(..), constStream, smap, zipWithS) 

import Control.Category
{-
Ответ:
http://stackoverflow.com/questions/21537010/where-haskell-category-composition-is-used-regardless-of-instance
Автомат Мили
-}
data St a b = St (a -> (b, St a b))
{-
instance Show a => Show (St a b) where
    show xs =  showInfinity (show (stake 5 xs))
        where showInfinity x = P.init x  P.++ "..."

-- Берёт из потока несколько первых элементов:
stake :: Int -> St a b -> [a]
stake n xxs@(St xs ) = htake n [] xxs
    where
        htake 0 acc _ = acc
        htake n acc (St xs ) = htake (n-1) (acc ++ [x]) xs
        -}
ap ∷ St a b -> [a] -> [b]
ap (St f) [] = []
ap (St f) (x:xs) = (fst (f x)): (ap (St f) xs)

instance Category St where
    id = St (\x -> (x, Control.Category.id))
    St bc. St ab = St $ \a -> case ab a of
            (b, nab) -> case bc b of
                (c, nbc) -> (c, nbc Control.Category.. nab)

const ∷ a -> St b a
const c = St (\x -> (c, Chapters.C5_St.const c))

integral ∷ Num a => St a a
integral = St (\x -> (x, Chapters.C5_St.integral))
