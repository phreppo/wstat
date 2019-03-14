module Equation.WideningPoints (buildWideningPoints) where

import WhileGrammar
import Equation.EquationList
import Tool.StateTransitions

--------------------------------------------------------------------------------
-- widening point set, using the loop heads
--------------------------------------------------------------------------------

buildWideningPoints :: Stmt -> [Label]
buildWideningPoints s =
  let (ws, _) = applyST (widen s) startingLabel in ws

-- hidden functions

widen :: Stmt -> ST [Label]

widen (Assign _ _) = do
  fresh
  used
  return []

widen (Assert _) = do
  fresh
  used
  return []

widen (Skip) = do
  fresh
  used
  return []

widen (Seq s1 s2) = do
  widen1 <- widen s1
  widen2 <- widen s2
  return $ widen1 ++ widen2

widen (If _ s1 s2) = do
  fresh
  used
  widen1 <- widen s1
  fresh
  used
  widen2 <- widen s2
  fresh
  used
  return $ widen1 ++ widen2

widen (While _ s) = do
  l1 <- fresh
  used
  widen1 <- widen s
  fresh
  used
  return $ l1:widen1

