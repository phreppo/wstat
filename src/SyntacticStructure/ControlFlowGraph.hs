module SyntacticStructure.ControlFlowGraph where

import Tool.StateTransitions
import Interfaces.AbstractStateDomain
import SyntacticStructure.WhileGrammar
import Semantic.Atomic
import Semantic.Condition
import Semantic.Statements
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

--------------------------------------------------------------------------------
--                       Control flow graph builder
--------------------------------------------------------------------------------

buildCfg :: ASD d => Stmt -> ControlFlowGraph (d -> d)
buildCfg stmt = let
    (cs, _) = applyST (cfg stmt stat condition) startingLabel in
        cs

cfg :: Stmt -> (Stmt -> a) -> (BExpr -> a) -> ST (ControlFlowGraph a)

cfg (Assign var expr) s _ = do
    label1 <- fresh
    label2 <- used
    return $ pure (label1, s $ Assign var expr, label2)

cfg (Assert cond) _ c = do
    label1 <- fresh
    label2 <- used
    return $ pure (label1, c cond, label2)

cfg (Skip) s _ = do
    label1 <- fresh
    label2 <- used
    return $ pure (label1, s Skip, label2)

cfg (Seq s1 s2) s c = do
    cfg1 <- cfg s1 s c
    cfg2 <- cfg s2 s c
    return $ cfg1 ++ cfg2

cfg (If cond s1 s2) s c = do
    label1 <- fresh
    label2 <- used
    cfg1 <- cfg s1 s c
    label3 <- fresh
    label4 <- used
    cfg2 <- cfg s2 s c
    label5 <- fresh
    label6 <- used
    return $ [
        (label1,c cond,label2),
        (label1,c $ BooleanUnary Not cond, label4),
        (label3,s Skip,label6),
        (label5,s Skip,label6)
        ] ++ cfg1 ++ cfg2

cfg (While cond stmt) s c = do
    label1 <- fresh
    label2 <- used 
    cfg1 <- cfg stmt s c
    label3 <- fresh
    label4 <- used
    return $ [
        (label1, c cond, label2),
        (label1, c $ BooleanUnary Not cond, label4),
        (label3, s Skip, label1)
        ] ++ cfg1

nextLabel :: Label -> Label
nextLabel = (+1)

startingLabel :: Label
startingLabel = 1

fresh :: ST Label
fresh = ST (\l -> (l, nextLabel l))

-- return the current label without compute anithing
used :: ST Label
used = ST (\l -> (l, l))