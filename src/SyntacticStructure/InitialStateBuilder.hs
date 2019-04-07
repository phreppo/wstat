{-# LANGUAGE FlexibleContexts #-}

module SyntacticStructure.InitialStateBuilder
    ( buildInitialSimpleSignState,
      buildInitialSignState,
      buildInitialIntervalState,
      readInitialIntervalState,
      readInitialSignState, 
      readInitialSimpleSignState )
where

import Data.Map
import Domains.IntervalDomain
import Domains.SignDomain
import Domains.SimpleSignDomain
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import Interfaces.State as S
import Semantic.EquationSolver
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.Parser
import SyntacticStructure.WhileGrammar
import System.Environment
import System.IO
import Tools.Utilities

readInitialIntervalState :: IO IntervalStateDomain
readInitialIntervalState =  readInitialGenericState

readInitialSignState :: IO SignStateDomain
readInitialSignState =  readInitialGenericState

readInitialSimpleSignState :: IO SimpleSignStateDomain
readInitialSimpleSignState =  readInitialGenericState

readInitialGenericState :: (AbstractValueDomain b, Read b) => IO (RelationalStateDomain String b)
readInitialGenericState =  do list <- readInitialStateAsList
                              return $ buildSmashedStateFromList list

buildSmashedStateFromList :: (CompleteLattice b, State RelationalStateDomain v b) => [(v, b)] -> RelationalStateDomain v b
buildSmashedStateFromList list = 
    if elem bottom ([snd x | x <- list]) 
        then Bottom 
        else S.fromList list

readInitialStateAsList :: (AbstractValueDomain b, Read b) => IO [(String, b)] 
readInitialStateAsList = do putStr "\t? variable: "
                            hFlush stdout
                            var <- readIdentifier
                            if var == "" 
                              then return []
                              else do
                                  putStr "\t  value:    "
                                  hFlush stdout

                                  val  <- readAbstractValue
                                  rest <- readInitialStateAsList
                                  return $ (var, val):rest

readIdentifier :: IO String 
readIdentifier = do s <- getLine
                    return s

readAbstractValue :: (AbstractValueDomain b, Read b) => IO b
readAbstractValue = do v <- getLine
                       return $ read v

buildInitialSimpleSignState :: Stmt -> SimpleSignStateDomain
buildInitialSimpleSignState = buildInitialState

buildInitialSignState :: Stmt -> SignStateDomain
buildInitialSignState = buildInitialState

buildInitialIntervalState :: Stmt -> IntervalStateDomain
buildInitialIntervalState = buildInitialState

buildInitialState :: AbstractValueDomain b => Stmt -> RelationalStateDomain Var b
buildInitialState abstractSyntaxTree = 
    RelationalStateDomain $ Data.Map.fromList $ 
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