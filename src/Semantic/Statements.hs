module Semantic.Statements where

import Interfaces.AbstractStateDomain
import SyntacticStructure.WhileGrammar
import Semantic.Atomic
import Semantic.Condition

stat :: ASD d => Stmt -> d -> d
stat (Assign var exp) = assign $ Assign var exp
stat (Skip) = id
-- stat (Assert c) = condition c
-- stat (Seq s1 s2) = (stat s2) . (stat s1)
-- ... other cases are implementing usign the Denotational style
