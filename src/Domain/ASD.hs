module Domain.ASD where

import Semantic.Atomic
import CompleteLattice

-- Abstract State Domain
class CompleteLattice d => ASD d where

    assign :: AtomicAssign -> d -> d

    cond :: AtomicCond -> d -> d