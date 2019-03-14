module Main where

import SyntacticStructure.WhileGrammar
import Parser.Parser
import System.IO
import SyntacticStructure.WideningPoints
import Semantic.Equational
import SyntacticStructure.ControlFlowGraph
import Interfaces.AbstractStateDomain
import Domain.StateDomainImplementation

-- TODO: remove these after refactoring statoinizialeeeeee
import Data.Map
import Domain.StateDomain
import Interfaces.CompleteLattice
import Domain.SimpleSign

main :: IO ()
main = do
    putStr "> Insert program source file name: "
    hFlush stdout
    programName <- getLine
    input <- readF programName
    putStrLn "=================================Program"
    print $ parse input
    putStrLn "==================================Result"
    let abstractSyntaxTree = parse input
        controlFlowGraph = buildCfg abstractSyntaxTree 
        wideningPoints = buildWideningPoints abstractSyntaxTree in 
        print $ fixpoint controlFlowGraph wideningPoints statoinizialeeeeeeeeeeeeeeeeeee (-1)
    return ()

readF :: String -> IO String
readF fileName = do
    inh <- openFile fileName ReadMode
    prog <- readloop inh
    hClose inh
    return prog

readloop :: Handle -> IO [Char]
readloop inh = do
    ineof <- hIsEOF inh
    if ineof
        then return []
        else do
            x <- hGetLine inh
            xs <- readloop inh
            return (x ++ xs)

statoinizialeeeeeeeeeeeeeeeeeee :: SD Var SimpleSign
statoinizialeeeeeeeeeeeeeeeeeee =
    SD $ fromList $ [("x",top)]

testProgram :: Stmt
testProgram = parse "x := 0; while x < 40 do x := x + 1 done"