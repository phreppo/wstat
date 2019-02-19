-- vscode + intero doesn't recognize Tasty module even stack test work

import Test.Tasty
import Test.Tasty.HUnit

import WhileProgramSpec

main :: IO ()
main = do
  defaultMain (testGroup "Library tests" tests)
    where
      tests = WhileProgramSpec.tests
              -- ++ add here specs