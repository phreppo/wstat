module SyntacticStructure.ControlFlowGraph where

import Interfaces.AbstractStateDomain
import Semantic.Atomic
import Semantic.AbstractSematic
import Semantic.AbstractSematic
import SyntacticStructure.WhileGrammar
import Tools.StateTransitions
import Tools.MonadicBuilder

--------------------------------------------------------------------------------
--                          Control Flow Graph Type
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

--------------------------------------------------------------------------------
--                       Control flow graph builder
--------------------------------------------------------------------------------

buildCfg :: AbstractStateDomain d => Stmt -> ControlFlowGraph (d -> d)
buildCfg stmt = let
    (cs, _) = applyST (cfg stmt calculateArcTransferFunction calculateArcCondition) startingLabel in
        cs

cfg :: Stmt -> (Stmt -> a) -> (BExpr -> a) -> ST (ControlFlowGraph a)
cfg stmt s c = cfgBuilderWithArgs stmt factory id ()
    where
        factory = [
            ASSIGN (\_ stmt l1 l2 -> pure (l1, s stmt, l2)),
            ASSERT (\_ (Assert cond) l1 l2 -> pure (l1, c cond, l2)),
            SKIP   (\_ _ l1 l2 -> pure (l1, s Skip, l2)),
            SEQ    (\_ _ -> (++)),
            IF     (\_ (If cond s1 s2) l1 l2 t l3 l4 f l5 l6 -> [
                (l1,c cond,l2),
                (l1,c $ BooleanUnary Not cond, l4),
                (l3,s Skip,l6),
                (l5,s Skip,l6)
              ] ++ t ++ f),
            WHILE  (\_ (While cond stmt) l1 l2 l3 x l4 l5 -> [
                (l1, s Skip, l2),
                (l2, c cond, l3),
                (l2, c $ BooleanUnary Not cond, l5),
                (l4, s Skip, l2)
                ] ++ x)
          ]
