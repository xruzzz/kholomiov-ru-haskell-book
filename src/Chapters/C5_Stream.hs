{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C5_Stream where
import qualified Prelude as P
import Prelude

import qualified Chapters.C4_Stream as C4 (Stream(..), constStream, smap,zipWithS) 

-- 5.2
instance Num a ⇒ Num (C4.Stream a) where
(+) = C4.zipWithS (P.+)
(*) = C4.zipWithS (P.*)
(-) = C4.zipWithS (P.-)

abs      = C4.smap P.abs
signum   = C4.smap P.signum
fromInteger x = C4.constStream x
