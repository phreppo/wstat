module Semantic.Evaluation where

import Domain.AVD as V
import Data.Map
import WhileGrammar
import Domain.SD

eval :: AVD b => AExpr -> SD b -> b
eval _         Bottom = V.bottom
eval (Var var) (SD x) = x ! var
eval (IntConst c) _      = cons c
eval (NonDet c1 c2) _ = rand c1 c2
eval (AUnary op e) x = unary op (eval e x)
eval (ABinary op e1 e2) x = binary op (eval e1 x) (eval e1 x)
