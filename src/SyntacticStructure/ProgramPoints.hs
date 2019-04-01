module SyntacticStructure.ProgramPoints (
  buildProgramPoints,
  buildWideningPoints) where

import SyntacticStructure.WhileGrammar
import SyntacticStructure.ControlFlowGraph
import Tools.StateTransitions
import Tools.Utilities
import Interfaces.AbstractStateDomain

buildProgramPoints :: ASD d => ControlFlowGraph (d -> d)  -> [Label]
buildProgramPoints controlFlowGraph =
    removeDuplicates $
        [ initialLabel | (initialLabel, function, finalLabel) <- controlFlowGraph ] ++
        [ finalLabel   | (initialLabel, function, finalLabel) <- controlFlowGraph ]

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
  fresh
  label1 <- fresh

  used
  wideningPointsInWhileBody <- buildWideningPointsMonadic s
  fresh
  used
  return $ label1:wideningPointsInWhileBody

