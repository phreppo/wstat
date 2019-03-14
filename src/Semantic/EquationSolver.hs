{-# LANGUAGE FlexibleContexts #-}

module Semantic.EquationSolver where

import SyntacticStructure.ControlFlowGraph
import Interfaces.CompleteLattice
import Interfaces.AbstractValueDomain
import Interfaces.AbstractStateDomain
import Domain.StateDomain
import SyntacticStructure.WhileGrammar
import Domain.SignDomain

fixpoint :: ASD d =>
    ControlFlowGraph (d -> d) ->
    [Label] ->
    d -> -- this will be top
    [(Label, d)]
fixpoint equations wideningPoints state =
    lub [systemResolver equations programPoints wideningPoints i state | i <- [0..]] (-1)
    where programPoints = calculateProgramPoints equations

calculateProgramPoints :: ASD d => ControlFlowGraph (d -> d)  -> [Label]
calculateProgramPoints equations = removeDuplicates $
        [ initialLabel | (initialLabel, function, finalLabel) <- equations ] ++
        [ finalLabel | (initialLabel, function, finalLabel) <- equations ]
    where removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

-- -1 to obtain the last possible
lub :: Eq a => [a] -> Integer -> a
lub (x:y:xs) 0 = x
lub (x:y:xs) i | x == y    = x
               | otherwise = lub (y:xs) (i-1)

systemResolver :: ASD d =>
    [CFGEdge (d -> d)] -> -- cfg
    [Label] -> -- program points
    [Label] -> -- widening points
    Int -> -- nth iteration
    d ->   -- initial state
    [(Label, d)]    -- state for every program point
systemResolver equations programPoints wideningPoints iteration initialState =
    [ (point, programPointNewStateCalculator point equations wideningPoints iteration initialState )  | point <- programPoints]

programPointNewStateCalculator :: ASD d =>
    Label -> -- program point
    [CFGEdge (d -> d)] -> -- list of equations
    [Label] -> -- widening points
    Int -> -- nth iteration
    d ->   -- initial state
    d      -- new state for the point
programPointNewStateCalculator 1 _ _ _ initialState = initialState
programPointNewStateCalculator _ _ _ 0 initialState = bottom
programPointNewStateCalculator j equations wideningPoints i initialState
        -- | j `elem` wideningPoints = (recCall j) `widen` (aaaaaaaaaaaaaaaaa entryProgramPoints)
        | False = bottom -- TODO: widening points
        | otherwise = foldr join bottom [ f $ programPointNewStateCalculator l0 equations wideningPoints (i-1) initialState | (l0, f, l1) <- entryProgramPoints ]
    where entryProgramPoints = [ (initialLabel, f, finalLabel) | (initialLabel, f, finalLabel) <- equations, finalLabel == j]