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

fixpoint :: (ASD d, Eq d) =>
    EqList Label (d -> d) ->
    [Label] ->
    d -> -- this will be top
    [(Label, d)]
fixpoint equations wideningPoints state =
    lub [ systemResolver eq programPoints wideningPoints i state | i <- [0..] ]
    where programPoints = calculateProgramPoints equations
          (eq, _) = equations

-- rmdups :: (Ord a) => [a] -> [a]
rmdups = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

calculateProgramPoints :: (ASD d, Eq d) => EqList Label (d -> d)  -> [Label]
calculateProgramPoints equations = rmdups 
        ([ initialLabel | (initialLabel, function, finalLabel) <- eq ] ++
        [ finalLabel | (initialLabel, function, finalLabel) <- eq ] )
    where eq = let (listOfProgramPointsAndFunctions, _) = equations in listOfProgramPointsAndFunctions

-- lub :: (ASD d, Eq d) => [d] -> d
lub (x:y:xs) | (x) == (y) = x
             | otherwise  = lub (y:xs)

systemResolver :: ASD d =>
    [Equation Label (d -> d)] -> -- cfg
    [Label] -> -- program points
    [Label] -> -- widening points
    Int -> -- nth iteration
    d ->   -- initial state
    [(Label, d)]    -- state for every program point
systemResolver _ programPoints _ 0 initialState = [ (point, firstState point initialState) | point <- programPoints]
systemResolver equations programPoints wideningPoints iteration initialState = 
    [ (point, newPointStateCalculator point equations wideningPoints iteration initialState )  | point <- programPoints] 

newPointStateCalculator :: ASD d => 
    Label -> -- program point
    [Equation Label (d -> d)] -> -- list of equations
    [Label] -> -- widening points
    Int -> -- nth iteration
    d ->   -- initial state
    d      -- new state for the point
newPointStateCalculator 1 _ _ _ initialState = initialState
newPointStateCalculator j equations wideningPoints i initialState 
        -- | j `elem` wideningPoints = (recCall j) `widen` (aaaaaaaaaaaaaaaaa enterPoints)
        | False = bottom -- TODO: widening points 
        | otherwise = systemResolver  
    where enterPoints = [ (initialLabel, f, finalLabel) | (initialLabel, f, finalLabel) <- equations, finalLabel == j] 

firstState :: (ASD d) => Label -> d -> d
firstState p initialState | p == 1 = initialState
                          | otherwise = bottom
-- systemResolver

-- equationProgress 1 iteration equation baseStateValue =

-- equationProgress programPoint iteration equation baseStateValue =

--------------------------------------------------------------------------------

fixpointComplete equations wideningPoints state = -- returns the entire table of partial results: use for debug purposes
    lubComplete [ systemResolver eq programPoints wideningPoints i state | i <- [0..] ]
    where programPoints = calculateProgramPoints equations
          (eq, _) = equations

lubComplete (x:y:xs) | (x) == (y) = (x:[y])
                     | otherwise  = x:(lubComplete (y:xs))