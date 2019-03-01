module Interfaces.AbstractStateDomain where

import Semantic.Atomic
import Interfaces.CompleteLattice

-- Abstract State Domain
class CompleteLattice d => ASD d where

    assign :: AtomicAssign -> d -> d

    cond :: AtomicCond -> d -> d