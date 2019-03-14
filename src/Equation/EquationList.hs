module Equation.EquationList where

import Tool.StateTransitions

--------------------------------------------------------------------------------
--                        Control Flow Graph Type 
--------------------------------------------------------------------------------

type Equation a = (Label, a, Label)

type ControlFlowGraph a = [Equation a]

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