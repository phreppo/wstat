module SyntacticStructure.InitialStateBuilder
    ( buildInitialSimpleSignState,
      buildInitialSignState,
      buildInitialIntervalState,
      readInitialIntervalState )
where

import Data.Map
import Domains.SimpleSignDomain
import Domains.SignDomain
import Domains.IntervalDomain
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import Interfaces.State as S
import Semantic.EquationSolver
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.Parser
import SyntacticStructure.WhileGrammar
import Tools.Utilities
import System.IO
import System.Environment

readInitialIntervalState :: IO IntervalStateDomain
readInitialIntervalState = do list <- readInitialIntervalStateAsList
                              return $ S.fromList list

readInitialIntervalStateAsList :: IO [(String, IntervalDomain)] 
readInitialIntervalStateAsList = do putStr "> var: "
                                    -- hFlush stdout
                                    System.IO.hIsTerminalDevice System.IO.stdin
                                    System.IO.hIsTerminalDevice System.IO.stdout
                                    System.Environment.getEnv "TERM"

                                    var <- readIdentifier
                                    if var == "" 
                                      then return []
                                      else
                                          do
                                          putStr "> val: "
                                          val  <- readAbstractValue
                                          rest <- readInitialIntervalStateAsList
                                          return $ (var, val):rest

readIdentifier :: IO String 
readIdentifier = do s <- getLine
                    return s

readAbstractValue :: (AVD b, Read b) => IO b
readAbstractValue = do v <- getLine
                       return $ read v

buildInitialSimpleSignState :: Stmt -> SimpleSignStateDomain
buildInitialSimpleSignState = buildInitialState

buildInitialSignState :: Stmt -> SignStateDomain
buildInitialSignState = buildInitialState

buildInitialIntervalState :: Stmt -> IntervalStateDomain
buildInitialIntervalState = buildInitialState

buildInitialState :: AVD b => Stmt -> SD Var b
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