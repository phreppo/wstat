module Equation.CfgBuilder where

import Equation.EquationList
import Domain.ASD
import WhileGrammar
import Semantic.Atomic
import Semantic.Condition


type F a = a -> a
buildCfg :: ASD d => Stmt -> Label -> EqList (F d)

-- base cases
buildCfg (Assign var expr) = buildEqSingleton $ assign $ AtomicAssign var expr
buildCfg (Assert c) = buildEqSingleton $ condition $ c
buildCfg (Skip) = buildEqSingleton id

-- recursive cases
buildCfg (Seq s1 s2) = seqLabelling s1 s2 buildCfg

buildCfg (If c s1 s2) = ifLabelling c s1 s2 buildCfg (\l1 l2 l3 l4 l5 l6 -> [
                          Equation (l1, condition $ c, l2),
                          Equation (l1, condition $ (BooleanUnary Not c), l4),
                          Equation (l3, id, l6),
                          Equation (l5, id, l6)
                        ])

buildCfg (While c s) = whileLabelling c s buildCfg (\l1 l2 l3 l4 l5 -> [
                          Equation (l1, id, l2),
                          Equation (l2, condition $ c, l3),
                          Equation (l2, condition $ (BooleanUnary Not c), l5),
                          Equation (l4, id, l2)
                        ])

-- TODO: use reasoning tecnique to remove append operator
seqLabelling :: Stmt -> Stmt -> (Stmt -> Label -> EqList a) -> Label -> EqList a
seqLabelling s1 s2 f l1 =  let EqList (xs', l2)   = f s1 l1
                               EqList (xs'', l3)  = f s2 l2 in EqList
                               (xs' ++ xs'', l3)

ifLabelling ::
  BExpr -> Stmt -> Stmt ->
  (Stmt -> Label -> EqList a) ->
  (Label -> Label -> Label -> Label -> Label -> Label -> [Equation a]) ->
  Label -> EqList a
ifLabelling c s1 s2 f g l1 = let l2                = nextLabel l1
                                 EqList (xs', l3)  = f s1 l2
                                 l4                = nextLabel l3
                                 EqList (xs'', l5) = f s2 l4
                                 l6                = nextLabel l5 in EqList
                                 ((g l1 l2 l3 l4 l5 l6) ++ xs' ++ xs'', l6)

whileLabelling ::
  BExpr -> Stmt ->
  (Stmt -> Label -> EqList a) ->
  (Label -> Label -> Label -> Label -> Label -> [Equation a]) ->
  Label -> EqList a
whileLabelling c s f g l1 = let l2              = nextLabel l1
                                l3              = nextLabel l2
                                EqList (xs, l4) = f s l3
                                l5              = nextLabel l4 in EqList
                                (g l1 l2 l3 l4 l5 ++ xs, l5)
