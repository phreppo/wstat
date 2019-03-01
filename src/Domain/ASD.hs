module Domain.ASD where

import Semantic.Atomic

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
    isBottom v = v `subset` bottom
