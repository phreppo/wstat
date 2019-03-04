module Main where

import WhileGrammar
import Parser.Parser
import System.IO

-- TODO: remove these after refactoring statoinizialeeeeee
import Data.Map
import Domain.StateDomain
import Interfaces.CompleteLattice
import Domain.SimpleSign

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

main :: IO ()
main = do 
    putStr "> Insert program source file name: "
    hFlush stdout
    programName <- getLine
    inp <- readF programName
    putStrLn "========================================"
    putStrLn $ show $ parse inp
    return ()

statoinizialeeeeeeeeeeeeeeeeeee :: SD V SimpleSign
statoinizialeeeeeeeeeeeeeeeeeee =
    SD $ fromList $ [("x",top), ("y",top)]