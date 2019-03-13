module Equation.CfgBuilder where

import Equation.EquationList
import Interfaces.AbstractStateDomain
import WhileGrammar
import Semantic.Atomic
import Semantic.Condition
import Tool.StateTransitions

buildCfg :: ASD d => Stmt -> Label -> EqList Label (d -> d)

-- base cases
buildCfg (Assign var expr) = buildEqSingleton $ assign $ AtomicAssign var expr
buildCfg (Assert c) = buildEqSingleton $ condition $ c
buildCfg (Skip) = buildEqSingleton id

-- recursive cases
buildCfg (Seq s1 s2) = seqLabelling s1 s2 buildCfg

buildCfg (If c s1 s2) = ifLabelling c s1 s2 buildCfg (\l1 l2 l3 l4 l5 l6 -> [
                          (l1, condition $ c, l2),
                          (l1, condition $ (BooleanUnary Not c), l4),
                          (l3, id, l6),
                          (l5, id, l6)
                        ])

buildCfg (While c s) = whileLabelling c s buildCfg (\l1 l2 l3 l4 l5 -> [
                          (l1, id, l2),
                          (l2, condition $ c, l3),
                          (l2, condition $ (BooleanUnary Not c), l5),
                          (l4, id, l2)
                        ])

-- TODO: use reasoning tecnique to remove append operator
seqLabelling ::
  Stmt -> Stmt -> (Stmt -> Label -> EqList Label a) -> Label -> EqList Label a
seqLabelling s1 s2 f l1 =  let (xs', l2)   = f s1 l1
                               (xs'', l3)  = f s2 l2 in
                               (xs' ++ xs'', l3)

ifLabelling :: BExpr -> Stmt -> Stmt ->
  (Stmt -> Label -> EqList Label a) ->
  (Label -> Label -> Label -> Label -> Label -> Label -> [Equation Label a]) ->
  Label -> EqList Label a
ifLabelling c s1 s2 f g l1 = let l2                = nextLabel l1
                                 (xs', l3)  = f s1 l2
                                 l4                = nextLabel l3
                                 (xs'', l5) = f s2 l4
                                 l6                = nextLabel l5 in
                                 ((g l1 l2 l3 l4 l5 l6) ++ xs' ++ xs'', l6)

whileLabelling :: BExpr -> Stmt ->
  (Stmt -> Label -> EqList Label a) ->
  (Label -> Label -> Label -> Label -> Label -> [Equation Label a]) ->
  Label -> EqList Label a
whileLabelling c s f g l1 = let l2              = nextLabel l1
                                l3              = nextLabel l2
                                (xs, l4) = f s l3
                                l5              = nextLabel l4 in
                                (g l1 l2 l3 l4 l5 ++ xs, l5)
