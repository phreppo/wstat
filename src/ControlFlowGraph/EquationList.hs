module ControlFlowGraph.EquationList where

import Tool.StateTransitions

--------------------------------------------------------------------------------
--                        Control Flow Graph Type 
--------------------------------------------------------------------------------
-- 
-- This module contains the representation of the control flow graph: 
-- it is a set of nodes (program points) with a list of oriented edges from 
-- one node to the another.
-- On every edge is stored the function that has to be applied when the abstract
-- interpretation computes the fixpoint.
-- The graph is represented as the set of its edges.
-- 

-- a is the type of the functions on edges
type ControlFlowGraph a = [CFGEdge a]

type CFGEdge a = (Label, a, Label)

type Label = Integer

nextLabel :: Label -> Label
nextLabel = (+1)

startingLabel :: Label
startingLabel = 1

fresh :: ST Label
fresh = ST (\l -> (l, nextLabel l))

-- return the current label without compute anithing
used :: ST Label
used = ST (\l -> (l, l))