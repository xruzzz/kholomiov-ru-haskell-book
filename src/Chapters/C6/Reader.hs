{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C6.Reader where
import Prelude as P hiding (id, (>>), (*>))

import Kleisli

-- 6.2
data Reader env b = Reader (env → b)

runReader :: Reader env b → (env → b)
runReader (Reader f) = f

instance Kleisli (Reader e) where
--      idK  :: a -> (Reader e) a
--      idK  :: a -> (Reader e) a
--      idK  :: a -> (env → b)
    idK x = Reader (\e → x)
-- (*>) :: (a → m b) → (b → m c) → (a → m c)
-- (*>) :: (a → (Reader e) b) → (b → (Reader e) c) → (a → (Reader e) c)
-- (*>) :: (a → (env → b)) → (b → (env → c)) → (a → (env → c))
    (*>) kl₁ kl₂ = \x₁ → Reader $ \e →
                        let x₂ = runReader (kl₁ x₁) e
                        in runReader (kl₂ x₂) e

instance Functor (Reader e) where
--  fmap :: (a → b) → Reader e a → Reader e b
    fmap f (Reader fx) = Reader $ \x → f (fx x)

instance Applicative (Reader e) where
    pure x = Reader (\e → x)
--    (<*>) :: Reader e (a → b) → Reader e a → Reader e b
--    (<*>) :: (env → (a → b))  → (env → a) → (env → b)
    (Reader fex) <*> (Reader fx) = Reader $ \e → fex e (fx e)

instance Monad (Reader e) where
    return = pure
--    (>>=) :: m a → (a → m b) → m b
--    (>>=) :: Reader e a → (a → Reader e b) → Reader e b
--    (>>=) :: (env → a)  → (a → (env → b)) → (env → b)
    (Reader fea) >>= k = Reader $ \e → (runReader (k (fea e))) e
