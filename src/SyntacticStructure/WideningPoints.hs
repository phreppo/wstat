module SyntacticStructure.WideningPoints (buildWideningPoints) where

import SyntacticStructure.WhileGrammar
import SyntacticStructure.ControlFlowGraph
import Tools.StateTransitions

--------------------------------------------------------------------------------
--                        Widening points calculator
--------------------------------------------------------------------------------
-- 
-- This moudle provides the method for calculating the widening points of a 
-- abstract syntax tree.
-- They correspond to the while constructs conditions.
-- 

buildWideningPoints :: Stmt -> [Label]
buildWideningPoints s =
  let (ws, _) = applyST (buildWideningPointsMonadic s) startingLabel in ws

buildWideningPointsMonadic :: Stmt -> ST [Label]
buildWideningPointsMonadic (Assign _ _) = do
  fresh
  used
  return []

buildWideningPointsMonadic (Assert _) = do
  fresh
  used
  return []

buildWideningPointsMonadic (Skip) = do
  fresh
  used
  return []

buildWideningPointsMonadic (Seq s1 s2) = do
  wideningPointsInS1 <- buildWideningPointsMonadic s1
  wideningPointsInS2 <- buildWideningPointsMonadic s2
  return $ wideningPointsInS1 ++ wideningPointsInS2

buildWideningPointsMonadic (If _ s1 s2) = do
  fresh
  used
  wideningPointsInThenBranch <- buildWideningPointsMonadic s1
  fresh
  used
  wideningPointsInElseBranch <- buildWideningPointsMonadic s2
  fresh
  used
  return $ wideningPointsInThenBranch ++ wideningPointsInElseBranch

buildWideningPointsMonadic (While _ s) = do
  label1 <- fresh
  used
  wideningPointsInWhileBody <- buildWideningPointsMonadic s
  fresh
  used
  return $ label1:wideningPointsInWhileBody

