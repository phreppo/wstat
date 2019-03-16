module SyntacticStructure.InitialStateBuilder
    ( buildInitialState )
where

import Domain.SignDomain
import Domain.StateDomain
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.State
import SyntacticStructure.Parser
import Semantic.EquationSolver
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.WhileGrammar
import Data.Map
import Interfaces.CompleteLattice
import Tools.Utilities

buildInitialState :: Stmt -> SD Var SignDomain
buildInitialState abstractSyntaxTree = 
    SD $ Data.Map.fromList $ 
        [ entry | entry <- (getIdentifiersInStmt abstractSyntaxTree) `zip` repeat top]

getIdentifiersInStmt :: Stmt -> [Var]
getIdentifiersInStmt stmt =
    (removeDuplicates . getIdentifiersInStmtWithDuplicates) stmt

getIdentifiersInStmtWithDuplicates :: Stmt -> [Var]
getIdentifiersInStmtWithDuplicates (Seq s1 s2) =
    (getIdentifiersInStmtWithDuplicates s1) ++ (getIdentifiersInStmtWithDuplicates s2)
getIdentifiersInStmtWithDuplicates (Assign identifier aexpr) =
    identifier:getIdentifiersInAexpr aexpr
getIdentifiersInStmtWithDuplicates (If bexpr then_stmt else_stmt) =
    (getIdentifiersInBexpr bexpr) ++ 
    (getIdentifiersInStmtWithDuplicates then_stmt) ++
    (getIdentifiersInStmtWithDuplicates else_stmt)
getIdentifiersInStmtWithDuplicates (While bexpr body) = 
    (getIdentifiersInBexpr bexpr) ++ 
    (getIdentifiersInStmtWithDuplicates body)
getIdentifiersInStmtWithDuplicates (Skip) = []
getIdentifiersInStmtWithDuplicates (Assert bexpr) =
    getIdentifiersInBexpr bexpr

getIdentifiersInAexpr :: AExpr -> [Var] 
getIdentifiersInAexpr (Var identifier) = [identifier]
getIdentifiersInAexpr (IntConst _)     = []
getIdentifiersInAexpr (AUnary _ aexpr) = getIdentifiersInAexpr aexpr
getIdentifiersInAexpr (ABinary _ aexpr1 aexpr2) = 
    (getIdentifiersInAexpr aexpr1) ++ (getIdentifiersInAexpr aexpr2)
getIdentifiersInAexpr (NonDet _ _) = []

getIdentifiersInBexpr :: BExpr -> [Var]
getIdentifiersInBexpr (BoolConst _)           = []
getIdentifiersInBexpr (BooleanUnary  _ bexpr) = getIdentifiersInBexpr bexpr
getIdentifiersInBexpr (BooleanBinary _ bexpr1 bexpr2) = 
    (getIdentifiersInBexpr bexpr1) ++ (getIdentifiersInBexpr bexpr2)
getIdentifiersInBexpr (ArithmeticBinary _ aexpr1 aexpr2) = 
    (getIdentifiersInAexpr aexpr1) ++ (getIdentifiersInAexpr aexpr2)