-- vscode + intero doesn't recognize Tasty module even stack test work

import Test.Tasty
import Test.Tasty.HUnit

import StmtParseSpec
import WhileProgramSpec

main :: IO ()
main = do
  defaultMain (testGroup "Library tests" tests)
    where
      tests = WhileProgramSpec.tests
              ++ StmtParseSpec.tests
              -- ++ add here specs