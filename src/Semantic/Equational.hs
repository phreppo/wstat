{-# LANGUAGE FlexibleContexts #-}

module Semantic.Equational where

import Equation.EquationList
import Interfaces.CompleteLattice
import Interfaces.AbstractValueDomain
import Interfaces.AbstractStateDomain
import Domain.StateDomain
import Domain.StateDomainImplementation
import WhileGrammar

fixpoint :: (ASD d, Eq d) =>
    EqList Label (d -> d) ->
    [Label] ->
    d -> -- this will be top
    [d]
fixpoint equations wideningPoints state =
    (⊓) [ systemResolver equations wideningPoints i state | i <- [0..] ]

(⊓) :: (ASD d, Eq d) => [[d]] -> [d]
(⊓) (x:y:xs) | (x) == (y) = x
             | otherwise  = (⊓) (y:xs)

systemResolver :: ASD d =>
    EqList Label (d -> d) -> -- cfg
    [Label] -> -- widening points
    Int -> -- nth iteration
    d ->   -- base state
    [d]    -- state for every program point
systemResolver _ _ 0 _ = [bottom]
systemResolver _ _ _ _ = [bottom]
-- systemResolver

-- equationProgress 1 iteration equation baseStateValue =

-- equationProgress programPoint iteration equation baseStateValue =