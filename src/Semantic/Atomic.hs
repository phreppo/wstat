module Semantic.Atomic where

import SyntacticStructure.WhileGrammar

data AtomicAssign = AtomicAssign String AExpr 
                  deriving Show

data AtomicCond   = AtomicCond BArithmeticBinOperator AExpr AExpr 
                  deriving Show