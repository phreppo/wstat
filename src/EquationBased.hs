{-# LANGUAGE FlexibleContexts #-}

module EquationBased where

import WhileGrammar
import Graph
import State
import Domain

-- -- generate Control Flow Graph from the given syntax tree

type Equation d = (Label, [d] -> [d], Label)

type Label = Integer

nextLabel :: Label -> Label
nextLabel = (+1)

buildEqSingleton :: ([d] -> [d]) -> Label -> ([Equation d], Label)
buildEqSingleton x l = ([(l, x, nextLabel l)], nextLabel l)

-- bad code TODO: Integer -> ([Equation *], Integer) should be a Monad
cfg :: Domain d => Stmt -> Label -> ([Equation d], Label)
cfg (Assign var expr) = buildEqSingleton $ assign $ AtomicAssign var expr
cfg (Assert c) = buildEqSingleton $ cond $ bexpr2atomic c
cfg (Skip) = buildEqSingleton id
cfg (Seq s1 s2) = \l1 -> let (xs', l2)   = cfg s1 l1
                             (xs'', l3)  = cfg s2 l2 in
                             (xs' ++ xs'', l3)
cfg (If c s1 s2) = \l1 -> let l2 = nextLabel l1
                              (xs', l3)  = cfg s1 l2
                              l4 = nextLabel l3
                              (xs'', l5) = cfg s2 l4
                              l6 = nextLabel l5 in
                              (
                                 (l1, cond $ bexpr2atomic c, l2):
                                 (l1, cond $ bexpr2atomic (Not c), l3):
                                 (l3, id, l6):
                                 (l5, id, l6):
                              xs' ++ xs'', l6)
cfg (While c s) = \l1 -> let l2 = nextLabel l1
                             l3 = nextLabel l2
                             (xs, l4) = cfg s l3
                             l5 = nextLabel l4 in
                             (
                                 (l1, id, l2):
                                 (l2, cond $ bexpr2atomic c, l3):
                                 (l2, cond $ bexpr2atomic (Not c), l5):
                                 (l4, id, l2):
                             xs, l5)

