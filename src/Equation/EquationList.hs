module Equation.EquationList where

import Domain.Domain
import Data.Monoid
import Control.Applicative

-- generate Control Flow Graph from the given syntax tree

--------------------------------------------------------------------------------
-- Equation Abstract Data Type
--------------------------------------------------------------------------------

type Label = Integer

data Equation a = Equation (Label, a, Label) deriving (Show, Eq)

-- TODO: make this a set
-- X list
data EqList a = EqList ([Equation a], Label) deriving Show

getCtx :: Equation a -> a
getCtx (Equation (_, x, _)) = x

instance Eq a => Eq (EqList a) where
  EqList (xs, lx) == EqList (ys, ly) =
    lx == ly &&
    length xs == length ys &&
    all (\x -> any (\y -> x == y) ys) xs &&
    all (\y -> any (\x -> x == y) xs) ys

nextLabel :: Label -> Label
nextLabel = (+1)

-- change using EQM
-- sigle step
buildEqSingleton :: a -> Label -> EqList a
buildEqSingleton x l = EqList ([Equation (l, x, nextLabel l)], nextLabel l)

--------------------------------------------------------------------------------
-- auxiliary show function for Equation
--------------------------------------------------------------------------------

showEquation :: Show a => [a] -> Equation (F a) -> (Label, [a], Label)
showEquation xs (Equation (l1, f, l2)) = (l1, f xs, l2)

showCFG :: Show a =>
           EqList (F a) -> [a] -> ([(Label, [a], Label)], Label)
showCFG (EqList (xs, lf)) ys = (map (showEquation ys) xs, lf)