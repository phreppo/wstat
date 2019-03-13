module Equation.MonadicCFG where

import Equation.EquationList
import Interfaces.AbstractStateDomain
import WhileGrammar
import Semantic.Atomic
import Semantic.Condition
import Tool.StateTransitions


fresh :: ST Label
fresh = ST (\l -> (l, nextLabel l))

used :: ST Label
used = ST (\l -> (l, l))

cfg :: Stmt -> (AtomicAssign -> a) -> (BExpr -> a) -> ST (EqList a)
cfg (Assign var expr) s c = do
    l1 <- fresh
    l2 <- used
    return [(l1,s (AtomicAssign var expr), l2)]
