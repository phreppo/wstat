module Semantic.Condition where

import Interfaces.CompleteLattice
import Interfaces.AbstractStateDomain
import Semantic.Atomic
import SyntacticStructure.WhileGrammar

condition :: ASD d => BExpr -> d -> d
condition (BoolConst True) = id
condition (BoolConst False) = const bottom
condition (BooleanBinary And c1 c2) =
  \d -> meet (condition c1 d) (condition c2 d)
condition (BooleanBinary Or c1 c2) =
  \d -> join (condition c1 d) (condition c2 d)
condition (ArithmeticBinary op c1 c2) = cond (AtomicCond op c1 c2) -- Atomic
condition (BooleanUnary Not c) = condition $ notRemover c -- possibly Atomic

--------------------------------------------------------------------------------
-- De Morgan laws, Not remover
--------------------------------------------------------------------------------

notRemover :: BExpr -> BExpr
notRemover (BooleanUnary Not c) = c
notRemover (BoolConst c) = boolConstLaws $ BoolConst c
notRemover (BooleanBinary op c1 c2) =
  BooleanBinary
    (boolOperatorLaws op)
    (BooleanUnary Not c1)
    (BooleanUnary Not c2)
notRemover (ArithmeticBinary op c1 c2) =
  ArithmeticBinary (arithmeticOperatorLaws op) c1 c2

arithmeticOperatorLaws :: BArithmeticBinOperator -> BArithmeticBinOperator
arithmeticOperatorLaws LessEq    = Greater
arithmeticOperatorLaws IsEqual   = IsNEqual
arithmeticOperatorLaws IsNEqual  = IsEqual
arithmeticOperatorLaws Less      = GreaterEq
arithmeticOperatorLaws Greater   = LessEq
arithmeticOperatorLaws GreaterEq = Less

boolOperatorLaws :: BBooleanBinOperator -> BBooleanBinOperator
boolOperatorLaws And = Or
boolOperatorLaws Or = And

boolConstLaws :: BExpr -> BExpr
boolConstLaws (BoolConst True) = BoolConst False
boolConstLaws (BoolConst False) = BoolConst True