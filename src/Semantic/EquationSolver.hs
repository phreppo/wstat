{-# LANGUAGE FlexibleContexts #-}

module Semantic.EquationSolver where

import Interfaces.AbstractStateDomain
import Interfaces.CompleteLattice
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.ProgramPoints

type ProgramPointState  st = (Label, st)
type ProgramPointsState st = [ProgramPointState st]

-- finds the fixpoint of the system of equations induced by the cfg and returns
-- one abstract state for every program point.
forwardAnalysis :: ASD d =>
    ControlFlowGraph (d -> d) ->
    [Label] ->           -- widening points
    -- (d -> d -> d) ->          -- convergence to terminate operator (widen or narrow operator)
    d ->                 -- initial state
    ProgramPointsState d -- final result: a state for every program point
forwardAnalysis cfg widenPoints initState =
    lub [ systemResolver cfg widenPoints i widen initState | i <- [0..]]

-- selects the first two equal states: the fixpoint
lub :: Eq a => [a] -> a
lub (x:y:xs) | x == y    = x
             | otherwise = lub (y:xs)

-- resolves the system of equations induced by the cfg at the nth iteration
systemResolver :: ASD d =>
    ControlFlowGraph (d -> d) ->
    [Label] ->            -- widening points
    Int ->                -- nth iteration var i
    (d -> d -> d) ->           -- widen or narrow operator
    d ->                  -- initial state
    ProgramPointsState d  -- state for every program point
systemResolver cfg widenPoints i op initState =
    [(programPoint, iterationResolver programPoint cfg widenPoints i op initState)
        | programPoint <- getProgramPoints cfg]

-- calculates the state of one program point at the nth iteration
iterationResolver :: ASD d =>
    Label ->                       -- program point
    ControlFlowGraph (d -> d) ->
    [Label] ->                     -- widening points
    Int ->                         -- nth iteration
    (d -> d -> d) ->               -- widening or narrowing operator
    d ->                           -- initial state
    d                              -- new state for the point
iterationResolver 1 _ _ _ _ initState = initState -- first program point
iterationResolver _ _ _ 0 _ initState = bottom    -- first iteration
iterationResolver j cfg widenPoints k op initState
        | j `elem` widenPoints = oldState `op` newState -- op == widen or narrow
        | otherwise = newState
    where
        entryProgramPoints = retrieveEntryLabels j cfg
        oldState = iterationResolver j cfg widenPoints (k-1) op initState
        newState = foldr join bottom
            [ f $ iterationResolver i cfg widenPoints (k-1) op initState
                | (i, f) <- entryProgramPoints ]

-- given a label and the entire cfg returns the entry label for each cfg-entries
-- that has as final label the given label
retrieveEntryLabels :: Label -> ControlFlowGraph a -> ProgramPointsState a
retrieveEntryLabels label cfg =
    [(initial, f) | (initial, f, final) <- cfg, final == label]


retrieveProgramPointState :: ProgramPointsState a -> Label -> a
retrieveProgramPointState [] label = error ("No program point " ++ show label ++ " found")
retrieveProgramPointState ((l, ctx):pps) label | label == l = ctx
                                               | otherwise  = retrieveProgramPointState pps label

applyNarrowing :: ASD d =>
    ControlFlowGraph (d -> d) ->
    [Label] ->              -- narrowing points
    d ->                    -- initial state
    ProgramPointsState d -> -- analysis result
    ProgramPointsState d
applyNarrowing cfg narrowingPoints initialState analysisResult = 
    lub [ systemResolverForNarrowing cfg narrowingPoints i narrow initialState analysisResult | i <- [0..]]

-- resolves the system of equations induced by the cfg at the nth iteration
systemResolverForNarrowing :: ASD d =>
    ControlFlowGraph (d -> d) ->
    [Label] ->            -- narrowing points
    Int ->                -- nth iteration var i
    (d -> d -> d) ->           -- widen or narrow operator
    d -> -- initial state
    ProgramPointsState d ->  -- analysis result
    ProgramPointsState d  -- state for every program point
systemResolverForNarrowing cfg narrowingPoints i op initialState analysisResult =
    [(programPoint, iterationResolverForNarrowing programPoint cfg narrowingPoints i op initialState analysisResult) 
        | programPoint <- getProgramPoints cfg]

-- calculates the state of one program point at the nth iteration
iterationResolverForNarrowing :: ASD d =>
    Label ->                       -- program point
    ControlFlowGraph (d -> d) ->
    [Label] ->                     -- widening points
    Int ->                         -- nth iteration
    (d -> d -> d) ->               -- widening or narrowing operator
    d -> -- initla state
    ProgramPointsState d -> -- analysis result
    d                              -- new state for the point
iterationResolverForNarrowing 1 _ _ _ _ initialState analysisResult = initialState -- first iteration
iterationResolverForNarrowing j _ _ 0 _ _ analysisResult = retrieveProgramPointState analysisResult j -- first iteration
iterationResolverForNarrowing j cfg narrowingPoints k op initialState analysisResult
        | j `elem` narrowingPoints = oldState `op` newState -- op == widen or narrow
        | otherwise = newState
    where
        entryProgramPoints = retrieveEntryLabels j cfg
        oldState = iterationResolverForNarrowing j cfg narrowingPoints (k-1) op initialState analysisResult
        newState = foldr join bottom
            [ f $ iterationResolverForNarrowing i cfg narrowingPoints (k-1) op initialState analysisResult
                | (i, f) <- entryProgramPoints ]