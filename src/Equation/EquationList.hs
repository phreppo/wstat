module Equation.EquationList where

import Domain.Domain
import Data.Monoid
import Control.Applicative

-- generate Control Flow Graph from the given syntax tree

--------------------------------------------------------------------------------
-- Equation Abstract Data Type
--------------------------------------------------------------------------------

-- Equation is generic in a to build in a simple way the relativeEquation monad
data Equation a = Equation (Label, a, Label) deriving (Show, Eq)

getCtx :: Equation a -> a
getCtx (Equation (_, x, _)) = x

data EqList a = EqList ([Equation a], Label) deriving Show

instance Eq a => Eq (EqList a) where
  EqList (xs, lx) == EqList (ys, ly) =
    lx == ly &&
    length xs == length ys &&
    all (\x -> any (\y -> x == y) ys) xs &&
    all (\y -> any (\x -> x == y) xs) ys

type Label = Integer

nextLabel :: Label -> Label
nextLabel = (+1)

-- change using EQM
buildEqSingleton :: ([d] -> [d]) -> Label -> EqList (F d)
buildEqSingleton x l = EqList ([Equation (l, x, nextLabel l)], nextLabel l)

--------------------------------------------------------------------------------
-- auxiliary show function for Equation
--------------------------------------------------------------------------------

showEquation :: Show a => [a] -> Equation (F a) -> (Label, [a], Label)
showEquation xs (Equation (l1, f, l2)) = (l1, f xs, l2)

showCFG :: Show a =>
           EqList (F a) -> [a] -> ([(Label, [a], Label)], Label)
showCFG (EqList (xs, lf)) ys = (map (showEquation ys) xs, lf)

--------------------------------------------------------------------------------
-- EQM is a Monads
--------------------------------------------------------------------------------

-- data EQM a = EQM (Label -> (a, Label)) -- a is EqList a

-- applyEQM :: EQM a -> Label -> (a, Label)
-- applyEQM (EQM f) l = f l

-- instance Functor EQM where
--   fmap f eqx = do x <- eqx
--                   return $ f x

-- instance Applicative EQM where
--   pure = return
--   eqf <*> eqx = do f <- eqf
--                    x <- eqx
--                    return $ f x

-- instance Monad EQM where
--   -- singleton, one step forward
--   return x = EQM (\l -> (x, nextLabel l))
--   -- (>>=) :: EQM a -> (a -> EQM b) -> EQM b
--   eqx >>= f = EQM (\l -> let (x, l') = applyEQM eqx l in
--                              applyEQM (f x) l')
