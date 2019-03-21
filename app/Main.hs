module Main where

import Interfaces.AbstractStateDomain
import SyntacticStructure.Parser
import Semantic.EquationSolver
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.WhileGrammar
import SyntacticStructure.ProgramPoints
import SyntacticStructure.InitialStateBuilder
import Domains.SimpleSignDomain
import Domains.SignDomain
import Tools.Utilities
import System.IO


main :: IO ()
main = do
    input <- readInput
    putStrLn "=================================Program"
    print $ parse input

    putStr "> Pick a domain: "
    hFlush stdout
    chosenDomain <- getLine

    runAnalysis chosenDomain (parse input)

runAnalysis :: String -> Stmt -> IO ()
runAnalysis domain abstractSyntaxTree = do
    let wideningPoints = buildWideningPoints abstractSyntaxTree in 
        case domain of 
            "ss" -> runSimpleSignDomainAnalysis abstractSyntaxTree wideningPoints
            "s"  -> runSignDomainAnalysis abstractSyntaxTree wideningPoints
            _    -> putStrLn ("Unknown domain " ++ show domain)

runSimpleSignDomainAnalysis:: Stmt -> [Label] -> IO () 
runSimpleSignDomainAnalysis abstractSyntaxTree wideningPoints = do
    print $ fixpoint controlFlowGraph wideningPoints (buildInitialSimpleSignState abstractSyntaxTree)
    return ()
    where controlFlowGraph = buildCfg abstractSyntaxTree

runSignDomainAnalysis :: Stmt -> [Label] -> IO () 
runSignDomainAnalysis abstractSyntaxTree wideningPoints = do
    print $ fixpoint controlFlowGraph wideningPoints (buildInitialSignState abstractSyntaxTree)
    return ()
    where controlFlowGraph = buildCfg abstractSyntaxTree