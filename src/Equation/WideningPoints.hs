module Equation.WideningPoints (wideningPoints) where

import WhileGrammar
import Equation.EquationList
import Equation.CfgBuilder

-- using the loop heads
wideningPoints :: Stmt -> Label -> [Label]
wideningPoints s l1  = let EqList (eqs, _) = wideningPoints' s l1 in
                           map (\(Equation (l, _, _)) -> l) eqs

-- hidden functions ------------------------------------------------------------

wideningPoints' :: Stmt -> Label -> EqList ()
wideningPoints' (Seq s1 s2) = seqLabelling s1 s2 wideningPoints'
wideningPoints' (If c s1 s2) = ifLabelling c s1 s2 wideningPoints' empty6
wideningPoints' (While c s) = whileLabelling c s wideningPoints' (
    \_ l2 _ _ _ -> [Equation (l2, (), l2)] -- l2 is the loop head
  )
wideningPoints' _ = \l1 -> EqList ([], nextLabel l1)


-- aux functions

empty6 :: Label -> Label -> Label -> Label -> Label -> Label -> [Equation a]
empty6 _ _ _ _ _ _ = []

empty5 :: Label -> Label -> Label -> Label -> Label -> [Equation a]
empty5 _ _ _ _ _ = []