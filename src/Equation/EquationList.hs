module Equation.EquationList where

import Domain.Domain

-- generate Control Flow Graph from the given syntax tree

--------------------------------------------------------------------------------
-- Equation Abstract Data Type
--------------------------------------------------------------------------------

-- Equation is generic in a to build in a simple way the relativeEquation monad
data Equation a = Equation (Label, a, Label)

data EqList a = EqList ([Equation a], Label)

type Label = Integer

nextLabel :: Label -> Label
nextLabel = (+1)

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
-- EqList is a Monads
--------------------------------------------------------------------------------

data EQM d = EQM (Label -> EqList d)