{-# LANGUAGE FlexibleContexts #-}

module Semantic.AbstractEvaluation where

import Data.Map
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.AbstractDomain
import Interfaces.State as S
import SyntacticStructure.WhileGrammar

abstractEval :: AbstractValueDomain b => AExpr -> NonRelationalStateDomain Var b -> b
abstractEval (Var var) = abstractEvalVar var
abstractEval (IntConst c) = \map -> cons c -- ignores the map
abstractEval (NonDet c1 c2) = \map -> rand c1 c2
abstractEval (AUnary op e) = (unary op) . abstractEval e
abstractEval (ABinary op e1 e2) =
    \map -> binary op (abstractEval e1 map) (abstractEval e2 map)

-- abstractEvalVar :: AbstractValueDomain b => String -> NonRelationalStateDomain Var b -> b
abstractEvalVar :: AbstractValueDomain b => String -> NonRelationalStateDomain Var b -> b
-- abstractEvalVar _   Bottom = bottom
abstractEvalVar var x      = S.lookup var x