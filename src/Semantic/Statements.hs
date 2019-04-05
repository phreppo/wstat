module Semantic.Statements where

import Interfaces.AbstractStateDomain
import Semantic.Atomic
import Semantic.Condition
import SyntacticStructure.WhileGrammar

stat :: ASD d => Stmt -> d -> d
stat (Assign var exp) = assign $ AtomicAssign var exp
stat (Skip) = id