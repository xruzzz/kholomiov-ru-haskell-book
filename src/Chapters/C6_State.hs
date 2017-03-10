{-# LANGUAGE UnicodeSyntax #-}
module Chapters.C6_State where
import Prelude as P hiding (id, (>>), (*>))

import Kleisli

-- 6.1
data State'' s a = State'' (s → (a, s))

runState :: State'' s a → s → (a, s)
runState (State'' f) = f

instance Kleisli (State'' s) where
    idK = \x → State'' (\s₁ → (x,s₁))
-- (*>) :: (a → m b) → (b → m c) → (a → m c)
-- (*>) :: (a → State'' s b) → (b → State'' s c) → (a → State'' s c)
-- (*>) :: (a → (s → (b, s))) → (b → (s → (c, s))) → (a → (s → (c, s)))
-- (*>) :: (a → (s → (b, s))) → (b → (s → (c, s))) → a → (s → (c, s))
    (*>) kl₁ kl₂ = \x₁ → State'' $ \s₁ →
                        let (x₂, s₂) = runState (kl₁ x₁) s₁
                        in runState (kl₂ x₂) s₂
    
instance Functor (State'' s) where
--  fmap :: (a → b) → State s a → State s b
    fmap f fx = State'' $ \s₁ → do
                                        let (State'' fs) = fx
                                        (f (fst (fs s₁)), snd (fs s₁))

instance Applicative (State'' s) where
    pure = \x → State'' (\y → (x,y))
--    (<*>) :: State s (a → b) → State s a → State s b
    (State'' h) <*> fx = State'' $ \s₁ → do
                                        let
                                            (f, s₂) = h s₁
                                            (State'' fs) = fx
                                        (f (fst (fs s₂)), snd (fs s₂))
instance Monad (State'' s) where
    return = \x → State'' (\s₁ → (x,s₁))
    (State'' h) >>= f = State'' $ \s₁ → let (a, s₂) = h s₁
                                    in runState (f a) s₂
