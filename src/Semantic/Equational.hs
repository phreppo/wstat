module Semantic.Equational where

import Equation.EquationList
import Interfaces.AbstractStateDomain
import Interfaces.CompleteLattice

systemResolver :: (Label l, ASD d) =>
    [Equation l (d -> d)] -> [l] -> Int -> d -> d
systemResolver _ _ 0 = const bottom
-- TODO: recursive case