module Equation.WideningPoints (wideningPoints) where

import WhileGrammar
import Equation.EquationList
import Equation.CfgBuilder

--------------------------------------------------------------------------------
-- widening point set, using the loop heads
--------------------------------------------------------------------------------

wideningPoints :: Label l => Stmt -> l -> [l]
wideningPoints s l1  = let EqList (eqs, _) = wideningPoints' s l1 in
                           map (\(Equation (l, _, _)) -> l) eqs

-- hidden functions ------------------------------------------------------------

-- TODO: refactor without eqlist
wideningPoints' :: Label l => Stmt -> l -> EqList l ()
wideningPoints' (Seq s1 s2) = seqLabelling s1 s2 wideningPoints'
wideningPoints' (If c s1 s2) = ifLabelling c s1 s2 wideningPoints' empty6
wideningPoints' (While c s) = whileLabelling c s wideningPoints'
  (\_ l2 _ _ _ -> [Equation (l2, (), l2)]) -- l2 is the loop head
wideningPoints' _ = \l1 -> EqList ([], nextLabel l1)


-- aux (hidden) functions

empty6 :: Label l => l -> l -> l -> l -> l -> l -> [Equation l a]
empty6 _ _ _ _ _ _ = []

empty5 :: Label l => l -> l -> l -> l -> l -> [Equation l a]
empty5 _ _ _ _ _ = []