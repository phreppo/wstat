{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Domains.IntervalDomain where

import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import Interfaces.State
import Semantic.Atomic
import Semantic.Evaluation
import SyntacticStructure.WhileGrammar

--------------------------------------------------------------------------------
--                             Sign Domain
--------------------------------------------------------------------------------

data IntervalDomain = BottomInterval
                    deriving (Show, Read, Eq, Ord, Enum)

instance CompleteLattice IntervalDomain where

    subset _  _  = True

    top    = BottomInterval
    bottom    = BottomInterval
    join _ _ = BottomInterval
    meet _ _ = BottomInterval
    widen = join

-- SignDomain is an Abstract Value Domain
instance AVD IntervalDomain where

    cons _ = BottomInterval
    rand _ _ = BottomInterval
    unary _ _       = BottomInterval
    binary _ _ _ = BottomInterval

instance ASD IntervalStateDomain where
    cond _ _ = Bottom
    -- assign :: AtomicAssign -> SD b -> SD b
    assign _ Bottom                  = Bottom
    assign (AtomicAssign var exp) x
        | isBottom $ abstractEval exp x = Bottom
        | otherwise                     = update var (abstractEval exp x) x


type IntervalStateDomain = SD Var IntervalDomain