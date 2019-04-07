module Semantic.Statements where

import Interfaces.AbstractStateDomain
import Semantic.Atomic
import Semantic.Condition
import SyntacticStructure.WhileGrammar

calculateArcTransferFunction :: AbstractStateDomain d => Stmt -> d -> d
calculateArcTransferFunction (Assign var exp) = assign $ AtomicAssign var exp
calculateArcTransferFunction (Skip) = id