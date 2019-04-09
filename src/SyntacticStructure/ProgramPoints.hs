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
        ASSIGN (\_ _ _ -> []),
        ASSERT (\_ _ _ -> []),
        SKIP   (\_ _ _ -> []),
        SEQ    (\_ -> (++)),
        IF     (\_ _ _ true _ _ false _ _ -> true ++ false),
        WHILE  (\_ _ l _ x _ _ -> l:x)
    ]

