{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C5_fix
    (
        mapFix,
        foldlLL,
        foldlFix
    ) where
import Data.Function (fix)
{- 5.5
map f []     = []
map f (x:xs) = f x : map f xs

xm []     = []
xm (x:xs) = f x : xm xs


xm = \inp -> case inp of
                  []     -> []
                  (x:xs) -> f x : xm xs

xm = (\t -> \inp -> case inp of
                  []     -> []
                  (x:xs) -> f x : t xs) xm 


xm = ff xm
    where ff = \t -> \inp -> case inp of
                  []     -> []
                  (x:xs) -> f x : t xs
-}

mapFix f = fix ff
    where ff t = \inp -> case inp of
                  []     -> []
                  (x:xs) -> f x : t xs

foldlLL :: (b->a->b)->b->[a]->b
foldlLL f ini [] = ini
foldlLL f ini (x:xs) = foldlLL f (f ini x) xs

{-
xm ini (x:xs) = xm (f ini x) xs
-}
foldlFix :: (b->a->b)->b->[a]->b
foldlFix f = fix ff
    where ff ft ini = \inp -> case inp of
                  []     -> ini
                  (x:xs) -> ft (f ini x) xs
