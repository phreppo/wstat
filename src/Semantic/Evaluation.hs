{-# LANGUAGE FlexibleContexts #-}

module Semantic.Evaluation where

import Interfaces.AbstractValueDomain as V
import Interfaces.AbstractStateDomain
import Interfaces.State as S
import Data.Map
import WhileGrammar
import Domain.StateDomain

abstractEval :: (State d V b, ASD (d V b), AVD b) => AExpr -> d V b -> b
abstractEval (Var var) = S.lookup var
abstractEval (IntConst c) = \map -> cons c -- ignores the map
abstractEval (NonDet c1 c2) = \map -> rand c1 c2
abstractEval (AUnary op e) = (unary op) . abstractEval e
abstractEval (ABinary op e1 e2) = 
    \map -> binary op (abstractEval e1 map) (abstractEval e1 map)
