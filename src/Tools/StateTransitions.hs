module Tools.StateTransitions where

--------------------------------------------------------------------------------
--                        State Transition Monad
--------------------------------------------------------------------------------
--
-- This module is used to building the CFG of the abstract syntax tree in
-- using the cabalities of the monads.
--

newtype ST a = ST (Label -> (a, Label))

type Label = Integer

applyST :: ST a -> Label -> (a, Label)
applyST (ST st) s = st s

instance Functor ST where
    fmap f st = do s <- st
                   return (f s)

instance Applicative ST where
    pure = return
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x)

instance Monad ST where
    return x = ST (\s -> (x, s))
    stx >>= f = ST (\s -> let (x, s') = applyST stx s in applyST (f x) s')


nextLabel :: Label -> Label
nextLabel = (+1)

startingLabel :: Label
startingLabel = 1

getNewLabelAndIncrement :: ST Label
getNewLabelAndIncrement = ST (\l -> (l, nextLabel l))

-- return the current label without compute anithing
getNewLabel :: ST Label
getNewLabel = ST (\l -> (l, l))