{-# LANGUAGE FlexibleContexts #-}

module Semantic.Equational where

import Equation.EquationList
import Interfaces.CompleteLattice
import Interfaces.AbstractValueDomain
import Interfaces.AbstractStateDomain
import Domain.StateDomain
import Domain.StateDomainImplementation
import WhileGrammar

fixpoint :: (Label l, ASD d, Eq d, Num l) =>
    EqList l (d -> d) ->
    [l] ->
    d -> -- this will be top
    [d]
fixpoint equations wideningPoints state =
    (⊓) [ systemResolver equations programPoints wideningPoints i state | i <- [0..] ]
    where eq = let (listOfProgramPointsAndFunctions, _) = equations in listOfProgramPointsAndFunctions
          programPoints = [ initialLabel | (initialLabel, function, finalLabel) <- eq ]

(⊓) :: (ASD d, Eq d) => [[d]] -> [d]
(⊓) (x:y:xs) | (x) == (y) = x
             | otherwise  = (⊓) (y:xs)

systemResolver :: (Label l, ASD d, Num l) =>
    EqList l (d -> d) -> -- list of equations
    [l] -> -- program points
    [l] -> -- widening points
    Int -> -- nth iteration
    d ->   -- base state
    [d]    -- state for every program point
systemResolver _ programPoints _ 0 initialState = [ firstState point initialState | point <- programPoints]
systemResolver _ _ _ _ _ = [bottom]

firstState p initialState | p == 1 = initialState
                          | otherwise = bottom
-- systemResolver

-- equationProgress 1 iteration equation baseStateValue =

-- equationProgress programPoint iteration equation baseStateValue =