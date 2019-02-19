module Semantic.Atomic where

import WhileGrammar

data AtomicAssign = AtomicAssign String AExpr deriving Show
data AtomicCond = AtomicCond BArithmeticBinOperator AExpr AExpr deriving Show