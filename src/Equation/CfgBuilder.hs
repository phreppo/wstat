module Equation.CfgBuilder where

import Equation.EquationList
import Domain.Domain
import WhileGrammar
import Semantic.Atomic
import Semantic.Condition


buildCfg :: Domain d => Stmt -> Label -> EqList d

-- base cases
buildCfg (Assign var expr) = buildEqSingleton $ assign $ AtomicAssign var expr
buildCfg (Assert c) = buildEqSingleton $ condition $ c
buildCfg (Skip) = buildEqSingleton id

-- recursive cases
buildCfg (Seq s1 s2) = \l1 -> let EqList (xs', l2)   = buildCfg s1 l1
                                  EqList (xs'', l3)  = buildCfg s2 l2 in
                                  EqList (xs' ++ xs'', l3)

buildCfg (If c s1 s2) = \l1 -> let l2         = nextLabel l1
                                   EqList (xs', l3)  = buildCfg s1 l2
                                   l4         = nextLabel l3
                                   EqList (xs'', l5) = buildCfg s2 l4
                                   l6         = nextLabel l5 in EqList (
                                      Equation (l1, condition $ c, l2):
                                      Equation (l1, condition $ (Not c), l4):
                                      Equation (l3, id, l6):
                                      Equation (l5, id, l6):
                                   xs' ++ xs'', l6)

buildCfg (While c s) = \l1 -> let l2       = nextLabel l1
                                  l3       = nextLabel l2
                                  EqList (xs, l4) = buildCfg s l3
                                  l5       = nextLabel l4 in EqList (
                                      Equation (l1, id, l2):
                                      Equation (l2, condition $ c, l3):
                                      Equation (l2, condition $ (Not c), l5):
                                      Equation (l4, id, l2):
                                  xs, l5)