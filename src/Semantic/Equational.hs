{-# LANGUAGE FlexibleContexts #-}

module Semantic.Equational where

import Equation.EquationList
import Interfaces.CompleteLattice
import Interfaces.AbstractValueDomain
import Interfaces.AbstractStateDomain
import Domain.StateDomain
import Domain.StateDomainImplementation
import WhileGrammar
import Domain.SimpleSign

fixpoint :: ASD d =>
    ControlFlowGraph (d -> d) ->
    [Label] ->
    d -> -- this will be top
    Integer ->
    [(Label, d)]
fixpoint equations wideningPoints state =
    lub [systemResolver equations programPoints wideningPoints i state | i <- [0..]]
    where programPoints = calculateProgramPoints equations

-- rmdups :: (Ord a) => [a] -> [a]
rmdups = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

calculateProgramPoints :: ASD d => ControlFlowGraph (d -> d)  -> [Label]
calculateProgramPoints equations = rmdups $
        [ initialLabel | (initialLabel, function, finalLabel) <- equations ] ++
        [ finalLabel | (initialLabel, function, finalLabel) <- equations ]

-- -1 to obtain the last possible
lub :: Eq a => [a] -> Integer -> a
lub (x:y:xs) 0 = x
lub (x:y:xs) i | x == y    = x
               | otherwise = lub (y:xs) (i-1)

systemResolver :: ASD d =>
    [Equation (d -> d)] -> -- cfg
    [Label] -> -- program points
    [Label] -> -- widening points
    Int -> -- nth iteration
    d ->   -- initial state
    [(Label, d)]    -- state for every program point
systemResolver equations programPoints wideningPoints iteration initialState =
    [ (point, newPointStateCalculator point equations wideningPoints iteration initialState )  | point <- programPoints]

newPointStateCalculator :: ASD d =>
    Label -> -- program point
    [Equation (d -> d)] -> -- list of equations
    [Label] -> -- widening points
    Int -> -- nth iteration
    d ->   -- initial state
    d      -- new state for the point
newPointStateCalculator 1 _ _ _ initialState = initialState
newPointStateCalculator _ _ _ 0 initialState = bottom
newPointStateCalculator j equations wideningPoints i initialState
        -- | j `elem` wideningPoints = (recCall j) `widen` (aaaaaaaaaaaaaaaaa enterPoints)
        | False = bottom -- TODO: widening points
        | otherwise = foldr join bottom [ f $ newPointStateCalculator l0 equations wideningPoints (i-1) initialState | (l0, f, l1) <- enterPoints ]
    where enterPoints = [ (initialLabel, f, finalLabel) | (initialLabel, f, finalLabel) <- equations, finalLabel == j]

-- systemResolver

-- equationProgress 1 iteration equation baseStateValue =

-- equationProgress programPoint iteration equation baseStateValue =

--------------------------------------------------------------------------------