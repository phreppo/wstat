{-# LANGUAGE FlexibleInstances #-}

module Domain.AbstractStateSignDomain where

import Domain.SignDomain
import Interfaces.AbstractValueDomain
import SyntacticStructure.WhileGrammar
import Interfaces.AbstractStateDomain
import Domain.StateDomain
import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import Interfaces.State
import Semantic.Atomic
import Semantic.Evaluation
import SyntacticStructure.WhileGrammar

instance ASD (SD Var SignDomain) where
    -- assign :: AtomicAssign -> SD b -> SD b
    assign _ Bottom                  = Bottom
    assign (AtomicAssign var exp) x
        | isBottom $ abstractEval exp x = Bottom
        | otherwise                     = update var (abstractEval exp x) x
    
    -- cond :: AtomicCond -> SD b -> SD b
    cond _ = id -- worst scenario