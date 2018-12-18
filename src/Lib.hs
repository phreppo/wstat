module Lib
where

import Control.Monad (replicateM)
    
import State
import WhileGrammar
import SugarRemover
import Parser
import StateParser
import WhilePrograms

someFunc :: IO ()
someFunc = putStrLn "someFunc"