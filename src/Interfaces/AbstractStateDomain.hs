module Interfaces.AbstractStateDomain where

import Semantic.Atomic
import Interfaces.CompleteLattice

--------------------------------------------------------------------------------
--                         Abstract State Domain
--------------------------------------------------------------------------------

class CompleteLattice d => ASD d where

    -- given one statement of assingment and one abstract state applies that assignment
    assign :: AtomicAssign -> d -> d

    -- given one condition statement and one abstract state applies that condition
    cond :: AtomicCond -> d -> d