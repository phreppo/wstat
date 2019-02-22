module Equation.EquationList where

import Domain.Domain

-- generate Control Flow Graph from the given syntax tree

--------------------------------------------------------------------------------
-- Equation Abstract Data Type
--------------------------------------------------------------------------------

data Equation d = Equation (Label, [d] -> [d], Label)

data EqList d = EqList ([Equation d], Label)

type Label = Integer

nextLabel :: Label -> Label
nextLabel = (+1)

buildEqSingleton :: ([d] -> [d]) -> Label -> EqList d
buildEqSingleton x l = EqList ([Equation (l, x, nextLabel l)], nextLabel l)

--------------------------------------------------------------------------------
-- auxiliary show function for Equation
--------------------------------------------------------------------------------

showEquation :: Show a => [a] -> Equation a -> (Label, [a], Label)
showEquation xs (Equation (l1, f, l2)) = (l1, f xs, l2)

showCFG :: Show a =>
           EqList a -> [a] -> ([(Label, [a], Label)], Label)
showCFG (EqList (xs, lf)) ys = (map (showEquation ys) xs, lf)

--------------------------------------------------------------------------------
-- EqList is a Monads
--------------------------------------------------------------------------------