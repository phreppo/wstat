module SyntacticStructure.InitialStateBuilder where

import Domain.SignDomain
import Domain.StateDomain
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.State
import Parser.Parser
import Semantic.Equational
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.WhileGrammar
import Data.Map
import Interfaces.CompleteLattice

-- this type will be :: Stmt -> IO (SD Var SignDomain) cause of random
buildInitialState :: Stmt -> SD Var SignDomain
buildInitialState abstractSyntaxTree = 
    SD $ Data.Map.fromList $ [("x",top)]

extractVariables :: Stmt -> [Var]
extractVariables _ = error "Extract variables: implement me!!"