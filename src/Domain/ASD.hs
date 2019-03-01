module Domain.ASD where

import Semantic.Atomic
import Domain.AVD
import WhileGrammar

-- Abstract State Domain
class ASD d where
    subset :: d -> d -> Bool

    bottom :: d

    assign :: AtomicAssign -> d -> d

    cond :: AtomicCond -> d -> d

    meet :: d -> d -> d

    join :: d -> d -> d

    widen :: d -> d -> d

    isBottom :: d -> Bool
    isBottom v = v `Domain.ASD.subset` Domain.ASD.bottom

    lookup :: AVD b => V -> d -> b

    update :: AVD b => V -> b -> d -> d
