module Tool.StateTransitions where

newtype ST s a = ST (s -> [(a, s)])

applyST :: ST s a -> s -> [(a, s)]
applyST (ST st) s = st s

instance Functor (ST s) where
    fmap f st = do s <- st
                   return (f s)

instance Applicative (ST s) where
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x)

instance Monad (ST s) where
    return x = ST (\s -> [(x, s)])
    stx >>= f = ST (\s -> case applyST stx s of
                              [] -> []
                              [(x, s')] -> applyST (f x) s')