{-# LANGUAGE FlexibleContexts #-}

module Semantic.EquationSolver where

import Interfaces.AbstractStateDomain
import Interfaces.CompleteLattice
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.ProgramPoints

type ProgramPointState  st = (Label, st)
type ProgramPointsState st = [ProgramPointState st]

search :: ProgramPointsState a -> Label -> a
search [] label = error "eroor while searching through program point list"
search ((l, ctx):pps) label | label == l = ctx
                            | otherwise  = search pps label

-- finds the fixpoint of the system of equations induced by the cfg and resturns
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
        entryProgramPoints = retrieveEntryLabel j cfg
        oldState = iterationResolver j cfg widenPoints (k-1) op initState
        newState = foldr join bottom
            [ f $ iterationResolver i cfg widenPoints (k-1) op initState
                | (i, f) <- entryProgramPoints ]

-- given a label and the entire cfg returns the entry label for each cfg-entries
-- that has as final label the given label
retrieveEntryLabel :: Label -> ControlFlowGraph a -> ProgramPointsState a
retrieveEntryLabel label cfg =
    [(initial, f) | (initial, f, final) <- cfg, final == label]