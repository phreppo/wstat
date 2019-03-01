{-# LANGUAGE FlexibleContexts #-}

module Semantic.Evaluation where

import Interfaces.AbstractValueDomain as V
import Interfaces.AbstractStateDomain
import Interfaces.State as S
import Data.Map
import WhileGrammar
import Domain.SD

eval :: (State d V b, ASD (d V b), AVD b) => AExpr -> d V b -> b
eval (Var var) = S.lookup var
eval (IntConst c) = const $ cons c
eval (NonDet c1 c2) = const $ rand c1 c2
eval (AUnary op e) = unary op . eval e
eval (ABinary op e1 e2) = \x -> binary op (eval e1 x) (eval e1 x)
