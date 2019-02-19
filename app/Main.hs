module Main where

import State
import WhileGrammar
import SugarRemover
import Parser.Parser
import Parser.StateParser

main :: IO ()
main =
  do
  print $ parse "x := 33; skip"
  putStrLn "someFunc"
