module Lib
where

import Control.Monad (replicateM)

import State
import WhileGrammar
import SugarRemover
import Parser.Parser
import Parser.StateParser
import WhilePrograms

someFunc :: IO ()
someFunc =
    do
    print $ parse "x := 33; skip"
    putStrLn "someFunc"

