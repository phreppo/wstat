module SyntacticStructure.ProgramPoints (
  getProgramPoints,
  chooseWideningPoints) where

import Interfaces.AbstractStateDomain
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.WhileGrammar
import Tools.StateTransitions
import Tools.Utilities

getProgramPoints :: AbstractStateDomain d => ControlFlowGraph (d -> d)  -> [Label]
getProgramPoints controlFlowGraph =
    removeDuplicates $
        [ initialLabel | (initialLabel, function, finalLabel) <- controlFlowGraph ] ++
        [ finalLabel   | (initialLabel, function, finalLabel) <- controlFlowGraph ]

-- choose the entry program point for each loop
chooseWideningPoints :: Stmt -> [Label]
chooseWideningPoints s =
  let (ws, _) = applyST (chooseWideningPointsMonadic s) startingLabel in ws

chooseWideningPointsMonadic :: Stmt -> ST [Label]
chooseWideningPointsMonadic (Assign _ _) = do
  getNewLabelAndIncrement
  getNewLabel
  return []

chooseWideningPointsMonadic (Assert _) = do
  getNewLabelAndIncrement
  getNewLabel
  return []

chooseWideningPointsMonadic (Skip) = do
  getNewLabelAndIncrement
  getNewLabel
  return []

chooseWideningPointsMonadic (Seq s1 s2) = do
  wideningPointsInS1 <- chooseWideningPointsMonadic s1
  wideningPointsInS2 <- chooseWideningPointsMonadic s2
  return $ wideningPointsInS1 ++ wideningPointsInS2

chooseWideningPointsMonadic (If _ s1 s2) = do
  getNewLabelAndIncrement
  getNewLabel
  wideningPointsInThenBranch <- chooseWideningPointsMonadic s1
  getNewLabelAndIncrement
  getNewLabel
  wideningPointsInElseBranch <- chooseWideningPointsMonadic s2
  getNewLabelAndIncrement
  getNewLabel
  return $ wideningPointsInThenBranch ++ wideningPointsInElseBranch

chooseWideningPointsMonadic (While _ s) = do
  label1 <- getNewLabelAndIncrement
  label2 <- getNewLabelAndIncrement
  label3 <- getNewLabel
  wideningPointsInWhileBody <- chooseWideningPointsMonadic s
  getNewLabelAndIncrement
  getNewLabel
  return $ label2:wideningPointsInWhileBody

