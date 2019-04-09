module SyntacticStructure.ProgramPoints (
  getProgramPoints,
  chooseWideningPoints) where

import Interfaces.AbstractStateDomain
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.WhileGrammar
import Tools.StateTransitions
import Tools.Utilities
import Tools.MonadicBuilder

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
chooseWideningPointsMonadic stmt = cfgBuilder stmt chooseWideningPointsFactory

chooseWideningPointsFactory :: CfgFactory [Label]
chooseWideningPointsFactory = [
        ASSIGN emptyFunc,
        ASSERT emptyFunc,
        SKIP   emptyFunc,
        SEQ    seqFunc,
        IF     ifFunc,
        WHILE  whileFunc
    ]

emptyFunc _ _ _ = []
seqFunc _ = (++)
ifFunc _ _ _ trueBlock _ _ falseBlock _ _ = trueBlock ++ falseBlock
whileFunc _ _ wideningPoint _ whileBody _ _ = wideningPoint:whileBody