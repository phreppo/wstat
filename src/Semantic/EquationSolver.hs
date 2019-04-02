{-# LANGUAGE FlexibleContexts #-}

module Semantic.EquationSolver where

import Domains.SimpleSignDomain
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import SyntacticStructure.ProgramPoints
import SyntacticStructure.WhileGrammar

type ProgramPointState  st = (Label, st)
type ProgramPointsState st = [ProgramPointState st]

-- finds the fixpoint of the system of equations induced by the cfg and resturns
-- one abstract state for every program point.
fixpoint :: ASD d =>
    ControlFlowGraph (d -> d) ->
    [Label] ->           -- widening points
    d ->                 -- initial state
    ProgramPointsState d -- final result: a state for every program point
fixpoint controlFlowGraph wideningPoints initialState =
    lub [ systemResolver controlFlowGraph programPoints wideningPoints i initialState | i <- [0..]]
    where programPoints = buildProgramPoints controlFlowGraph

-- selects the first two equal states: the fixpoint
lub :: Eq a => [a] -> a
lub (x:y:xs) | x == y    = x
             | otherwise = lub (y:xs)

-- resolves the system of equations induced by the cfg at the nth iteration
systemResolver :: ASD d =>
    ControlFlowGraph (d -> d) ->
    [Label] ->            -- program points
    [Label] ->            -- widening points
    Int ->                -- nth iteration
    d ->                  -- initial state
    ProgramPointsState d  -- state for every program point
systemResolver controlFlowGraph programPoints wideningPoints iteration initialState =
    [ (programPoint, programPointNewStateCalculator programPoint controlFlowGraph wideningPoints iteration initialState )
        | programPoint <- programPoints]

-- calculates the state of one program point at the nth iteration
programPointNewStateCalculator :: ASD d =>
    Label ->                       -- program point
    ControlFlowGraph (d -> d) ->
    [Label] ->                     -- widening points
    Int ->                         -- nth iteration
    d ->                           -- initial state
    d                              -- new state for the point
programPointNewStateCalculator 1 _ _ _ initialState = initialState -- first program point
programPointNewStateCalculator _ _ _ 0 initialState = bottom       -- first iteration
programPointNewStateCalculator programPoint cfg wideningPoints i initialState
        | programPoint `elem` wideningPoints = programPointOldAbstractState `widen` programPointNewAbstractState
        | otherwise = programPointNewAbstractState
    where entryProgramPoints = [ (initialLabel, f, finalLabel) | (initialLabel, f, finalLabel) <- cfg, finalLabel == programPoint]
          programPointOldAbstractState = programPointNewStateCalculator programPoint cfg wideningPoints (i-1) initialState
          programPointNewAbstractState = foldr join bottom [ f $ programPointNewStateCalculator l0 cfg wideningPoints (i-1) initialState | (l0, f, l1) <- entryProgramPoints ]
