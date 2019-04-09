module Semantic.AbstractSematic where

import Interfaces.AbstractStateDomain
import Interfaces.CompleteLattice
import Semantic.Atomic
import SyntacticStructure.WhileGrammar

calculateArcTransferFunction :: AbstractStateDomain d => Stmt -> d -> d
calculateArcTransferFunction (Assign var exp) = assign $ AtomicAssign var exp
calculateArcTransferFunction (Skip) = id

calculateArcCondition :: AbstractStateDomain d => BExpr -> d -> d
calculateArcCondition (BoolConst True) = id
calculateArcCondition (BoolConst False) = const bottom
calculateArcCondition (BooleanBinary And c1 c2) =
  \d -> meet (calculateArcCondition c1 d) (calculateArcCondition c2 d)
calculateArcCondition (BooleanBinary Or c1 c2) =
  \d -> join (calculateArcCondition c1 d) (calculateArcCondition c2 d)
calculateArcCondition (ArithmeticBinary op c1 c2) = cond (AtomicCond op c1 c2) -- Atomic
calculateArcCondition (BooleanUnary Not c) = calculateArcCondition $ notRemover c -- possibly Atomic

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