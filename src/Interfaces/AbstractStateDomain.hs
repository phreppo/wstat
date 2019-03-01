module Interfaces.AbstractStateDomain where

import Semantic.Atomic
import Interfaces.CompleteLattice

--------------------------------------------------------------------------------
-- Abstract State Domain
--------------------------------------------------------------------------------

-- b is a powerset of abstract states
class CompleteLattice d => ASD d where

    assign :: AtomicAssign -> d -> d

    cond :: AtomicCond -> d -> d