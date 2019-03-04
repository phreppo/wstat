{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Domain.StateDomainImplementation where

import Data.Map                       (keys)
import Interfaces.AbstractValueDomain (AVD)
import Interfaces.CompleteLattice     (CompleteLattice(..))
import Interfaces.State               (update)
import Interfaces.AbstractStateDomain (ASD(..))
import Semantic.Atomic                (AtomicAssign(..))
import Semantic.Evaluation            (abstractEval)
import WhileGrammar                   (V)
import Domain.StateDomain
                                (SD(..), mergeStateDomainsWith, mergeWithFunction, applyFunction, applyPredicate)

--------------------------------------------------------------------------------
-- Abstract State Domain, implementation using the Abstract Value Domain passed
--------------------------------------------------------------------------------

-- SD is a complete lattice
instance AVD b => CompleteLattice (SD V b) where

    -- bottom :: SD b
    bottom = Bottom

    -- subset :: SD b -> Sb d -> Bool
    subset Bottom _      = True
    subset _      Bottom = False
    subset (SD x) (SD y) = all (applyPredicate subset x y) (keys x)

    -- meet :: SD b -> SD b -> SD b
    meet Bottom _ = Bottom
    meet _ Bottom = Bottom
    meet (SD x) (SD y)
        | any (isBottom . applyFunction meet x y) (keys x) = Bottom -- bottom smashing
        | otherwise = mergeWithFunction meet x y

    -- join :: SD b -> SD b -> SD b
    join = mergeStateDomainsWith join

    -- widen :: SD b -> SD b -> SD b
    widen = mergeStateDomainsWith widen

-- SD is an Abstract State Domain
instance AVD b => ASD (SD V b) where

    -- assign :: AtomicAssign -> SD b -> SD b
    assign _ Bottom                  = Bottom
    assign (AtomicAssign var exp) x
        | isBottom $ abstractEval exp x = Bottom
        | otherwise                     = update var (abstractEval exp x) x

    -- cond :: AtomicCond -> SD b -> SD b
    cond _ = id -- worst scenario
