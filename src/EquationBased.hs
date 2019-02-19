{-# LANGUAGE FlexibleContexts #-}

module EquationBased where


import WhileGrammar
import Semantic.Condition
import Domain.Domain
import Semantic.Atomic

-- generate Control Flow Graph from the given syntax tree

--------------------------------------------------------------------------------
-- Equation Abstract Data Type
--------------------------------------------------------------------------------

type Equation d = (Label, [d] -> [d], Label)

type EqList d = ([Equation d], Label)

type Label = Integer

nextLabel :: Label -> Label
nextLabel = (+1)

buildEqSingleton :: ([d] -> [d]) -> Label -> EqList d
buildEqSingleton x l = ([(l, x, nextLabel l)], nextLabel l)

--------------------------------------------------------------------------------
-- cfg builder definition
--------------------------------------------------------------------------------

cfgBuilder :: Domain d => Stmt -> Label -> EqList d

-- base cases
cfgBuilder (Assign var expr) = buildEqSingleton $ assign $ AtomicAssign var expr
cfgBuilder (Assert c) = buildEqSingleton $ condition $ c
cfgBuilder (Skip) = buildEqSingleton id

-- recursive cases
cfgBuilder (Seq s1 s2) = \l1 -> let (xs', l2)   = cfgBuilder s1 l1
                                    (xs'', l3)  = cfgBuilder s2 l2 in
                                    (xs' ++ xs'', l3)

cfgBuilder (If c s1 s2) = \l1 -> let l2         = nextLabel l1
                                     (xs', l3)  = cfgBuilder s1 l2
                                     l4         = nextLabel l3
                                     (xs'', l5) = cfgBuilder s2 l4
                                     l6         = nextLabel l5 in (
                                        (l1, condition $ c, l2):
                                        (l1, condition $ (Not c), l4):
                                        (l3, id, l6):
                                        (l5, id, l6):
                                     xs' ++ xs'', l6)

cfgBuilder (While c s) = \l1 -> let l2       = nextLabel l1
                                    l3       = nextLabel l2
                                    (xs, l4) = cfgBuilder s l3
                                    l5       = nextLabel l4 in (
                                        (l1, id, l2):
                                        (l2, condition $ c, l3):
                                        (l2, condition $ (Not c), l5):
                                        (l4, id, l2):
                                    xs, l5)

--------------------------------------------------------------------------------
-- auxiliary show function for Equation
--------------------------------------------------------------------------------

showEquation :: Show a => [a] -> Equation a -> (Label, [a], Label)
showEquation xs (l1, f, l2) = (l1, f xs, l2)

showCFG :: Show a =>
           ([Equation a], Label) -> [a] -> ([(Label, [a], Label)], Label)
showCFG (xs, lf) ys = (map (showEquation ys) xs, lf)
