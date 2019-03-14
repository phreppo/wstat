module Main where

import Interfaces.AbstractStateDomain
import Parser.Parser
import Semantic.EquationSolver
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.WhileGrammar
import SyntacticStructure.WideningPoints
import SyntacticStructure.InitialStateBuilder
import System.IO


main :: IO ()
main = do
    input <- readInput
    putStrLn "=================================Program"
    print $ parse input
    putStrLn "==================================Result"
    let abstractSyntaxTree = parse input
        controlFlowGraph   = buildCfg abstractSyntaxTree 
        wideningPoints     = buildWideningPoints abstractSyntaxTree
        initialState       = buildInitialState abstractSyntaxTree in 

        print $ fixpoint controlFlowGraph wideningPoints initialState

readInput :: IO String 
readInput = do 
    putStr "> Insert program source file name: "
    hFlush stdout
    programName <- getLine
    input <- readF programName
    return input

readF :: String -> IO String
readF fileName = do
    inh <- openFile fileName ReadMode
    prog <- readLoop inh
    hClose inh
    return prog

readLoop :: Handle -> IO [Char]
readLoop inh = do
    ineof <- hIsEOF inh
    if ineof
        then return []
        else do
            x <- hGetLine inh
            xs <- readLoop inh
            return (x ++ xs)