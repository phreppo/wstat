module Main where

import WhileGrammar
import Parser.Parser
import System.IO
import Equation.CfgBuilder
import Equation.WideningPoints
import Semantic.Equational
import Equation.EquationList
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
    inp <- readF programName
    putStrLn "========================================"
    putStrLn $ show $ parse inp
    putStrLn "========================================"
    putStrLn $ show $ fixpoint (buildCfg (parse inp) (1::Integer)) (wideningPoints (parse inp) 1) statoinizialeeeeeeeeeeeeeeeeeee
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
    SD $ fromList $ [("x",top), ("y",top)]