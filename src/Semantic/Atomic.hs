module Semantic.Atomic where

import SyntacticStructure.WhileGrammar

data AtomicCond   = AtomicCond BArithmeticBinOperator AExpr AExpr
                  deriving Show