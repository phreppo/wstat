{-# LANGUAGE FlexibleContexts #-}

module Main where

import Domains.DomainsList
import Domains.IntervalDomain
import Domains.SignDomain
import Domains.SimpleSignDomain
import Domains.CongruenceDomain
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Semantic.EquationSolver
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.InitialStateBuilder
import SyntacticStructure.Parser
import SyntacticStructure.PrettyPrinter
import SyntacticStructure.ProgramPoints
import SyntacticStructure.WhileGrammar
import System.IO
import Tools.Utilities
import Tools.StateTransitions


main :: IO ()
main = do
    input <- readInput
    -- putStrLn "=================================Program"
    -- print $ parse input

    putStr $ "> Pick a domain in ["++ listAllDomains ++"]: "
    hFlush stdout
    chosenDomain <- getLine

    runAnalysis chosenDomain (parse input)

runAnalysis :: String -> Stmt -> IO ()
runAnalysis domain abstractSyntaxTree = do
    let wideningPoints = chooseWideningPoints abstractSyntaxTree in
        case domain of
            "ss" -> runSimpleSignDomainAnalysis abstractSyntaxTree wideningPoints
            "s"  -> runSignDomainAnalysis       abstractSyntaxTree wideningPoints
            "i"  -> runIntervalDomainAnalysis   abstractSyntaxTree wideningPoints
            "c"  -> runCongruenceDomainAnalysis abstractSyntaxTree wideningPoints
            _    -> putStrLn ("Unknown domain " ++ show domain)

runSimpleSignDomainAnalysis:: Stmt -> [Label] -> IO ()
runSimpleSignDomainAnalysis abstractSyntaxTree wideningPoints =
    runGenericAnalysis abstractSyntaxTree wideningPoints readInitialSimpleSignState buildInitialSimpleSignState

runSignDomainAnalysis :: Stmt -> [Label] -> IO ()
runSignDomainAnalysis abstractSyntaxTree wideningPoints =
    runGenericAnalysis abstractSyntaxTree wideningPoints readInitialSignState buildInitialSignState

runIntervalDomainAnalysis :: Stmt -> [Label] -> IO ()
runIntervalDomainAnalysis abstractSyntaxTree wideningPoints = do
    runGenericAnalysis abstractSyntaxTree wideningPoints readInitialIntervalState buildInitialIntervalState

runCongruenceDomainAnalysis :: Stmt -> [Label] -> IO ()
runCongruenceDomainAnalysis abstractSyntaxTree wideningPoints = do
    runGenericAnalysis abstractSyntaxTree wideningPoints readInitialCongruenceState buildInitialCongruenceState

runGenericAnalysis :: (AbstractStateDomain (NonRelationalStateDomain Var b), AbstractValueDomain b) =>
       Stmt ->
       [Label] ->
       IO (NonRelationalStateDomain Var b) ->
       (Stmt ->
       NonRelationalStateDomain Var b) ->
       IO ()
runGenericAnalysis abstractSyntaxTree wideningPoints initialStateReader initialStateBuilder = do
    userState <- readInitialState initialStateReader
    putStr "> State: "
    print userState
    hFlush stdout
    let controlFlowGraph = buildCfg abstractSyntaxTree
        defaultState     = initialStateBuilder abstractSyntaxTree
        initialState     = overrideStates userState defaultState
        analysisResult   = analyze controlFlowGraph wideningPoints initialState in
        putStr $ prettyPrint abstractSyntaxTree analysisResult

readInitialState :: IO d -> IO d
readInitialState reader = do putStrLn "> Insert initial map (just return to complete the process):"
                             userState <- reader
                             return userState