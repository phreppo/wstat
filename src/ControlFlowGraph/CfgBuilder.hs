module ControlFlowGraph.CfgBuilder (buildCfg, cfg) where

import ControlFlowGraph.EquationList
import Interfaces.AbstractStateDomain
import WhileGrammar
import Semantic.Atomic
import Semantic.Condition
import Semantic.Statements
import Tool.StateTransitions

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
