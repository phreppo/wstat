module Main where

import WhileGrammar
import Parser.Parser

main :: IO ()
main =
  do
  print $ parse "x := 33; skip"
  putStrLn "someFunc"
