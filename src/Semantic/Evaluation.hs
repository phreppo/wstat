{-# LANGUAGE FlexibleContexts #-}

module Semantic.Evaluation where

import Interfaces.AbstractValueDomain as V
import Interfaces.AbstractStateDomain
import Interfaces.State as S
import Data.Map
import SyntacticStructure.WhileGrammar

abstractEval :: (State d Var b, ASD (d Var b), AVD b) => AExpr -> d Var b -> b
abstractEval (Var var) = abstractEvalVar var
abstractEval (IntConst c) = \map -> cons c -- ignores the map
abstractEval (NonDet c1 c2) = \map -> rand c1 c2
abstractEval (AUnary op e) = (unary op) . abstractEval e
abstractEval (ABinary op e1 e2) =
    \map -> binary op (abstractEval e1 map) (abstractEval e2 map)

abstractEvalVar :: (State d Var b, ASD (d Var b), AVD b) => Var -> d Var b -> b
abstractEvalVar var = S.lookup var