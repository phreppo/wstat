module Equation.MonadicCfg (buildCfg) where

import Equation.EquationList
import Interfaces.AbstractStateDomain
import WhileGrammar
import Semantic.Atomic
import Semantic.Condition
import Semantic.Statements
import Tool.StateTransitions

buildCfg :: ASD d => Stmt -> EqList (d -> d)
buildCfg stmt = let
    (cs, _) = applyST (cfg stmt stat condition) startingLabel in
        cs

cfg :: Stmt -> (Stmt -> a) -> (BExpr -> a) -> ST (EqList a)

cfg (Assign var expr) s _ = do
    l1 <- fresh
    l2 <- used
    return $ pure (l1, s $ Assign var expr, l2)

cfg (Assert cond) _ c = do
    l1 <- fresh
    l2 <- used
    return $ pure (l1, c cond, l2)

cfg (Skip) s _ = do
    l1 <- fresh
    l2 <- used
    return $ pure (l1, s Skip, l2)

cfg (Seq s1 s2) s c = do
    cfg1 <- cfg s1 s c
    cfg2 <- cfg s2 s c
    return $ cfg1 ++ cfg2

cfg (If cond s1 s2) s c = do
    l1 <- fresh
    l2 <- used
    cfg1 <- cfg s1 s c
    l3 <- fresh
    l4 <- used
    cfg2 <- cfg s2 s c
    l5 <- fresh
    l6 <- used
    return $ [
        (l1,c cond,l2),
        (l1,c $ BooleanUnary Not cond, l4),
        (l3,s Skip,l6),
        (l5,s Skip,l6)
        ] ++ cfg1 ++ cfg2

cfg (While cond stmt) s c = do
    l1 <- fresh
    l2 <- used
    cfg1 <- cfg stmt s c
    l3 <- fresh
    l4 <- used
    return $ [
        (l1, c cond, l2),
        (l1, c $ BooleanUnary Not cond, l4),
        (l3, s Skip, l1)
        ] ++ cfg1

