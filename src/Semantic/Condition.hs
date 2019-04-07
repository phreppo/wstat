module Semantic.Condition where

import Interfaces.AbstractStateDomain
import Interfaces.CompleteLattice
import Semantic.Atomic
import SyntacticStructure.WhileGrammar

condition :: AbstractStateDomain d => BExpr -> d -> d
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
notRemover (BoolConst c) = negateBooleanConst $ BoolConst c
notRemover (BooleanBinary op c1 c2) =
  BooleanBinary
    (negateBooleanOperator op)
    (BooleanUnary Not c1)
    (BooleanUnary Not c2)
notRemover (ArithmeticBinary op c1 c2) =
  ArithmeticBinary (negateRelationalOperator op) c1 c2

negateRelationalOperator :: BArithmeticBinOperator -> BArithmeticBinOperator
negateRelationalOperator LessEq    = Greater
negateRelationalOperator IsEqual   = IsNEqual
negateRelationalOperator IsNEqual  = IsEqual
negateRelationalOperator Less      = GreaterEq
negateRelationalOperator Greater   = LessEq
negateRelationalOperator GreaterEq = Less

negateBooleanOperator :: BBooleanBinOperator -> BBooleanBinOperator
negateBooleanOperator And = Or
negateBooleanOperator Or = And

negateBooleanConst :: BExpr -> BExpr
negateBooleanConst (BoolConst True) = BoolConst False
negateBooleanConst (BoolConst False) = BoolConst True