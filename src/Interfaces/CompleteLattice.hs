module Interfaces.CompleteLattice where

--------------------------------------------------------------------------------
--                               Complete Lattice
--------------------------------------------------------------------------------

class Eq p => CompleteLattice p where

    subset :: p -> p -> Bool

    top :: p

    bottom :: p

    join :: p -> p -> p

    meet :: p -> p -> p

    widen :: p -> p -> p

    narrow :: p -> p -> p

    -- auxiliary functions

    isBottom :: p -> Bool
    isBottom v = v `subset` bottom
    -- complete lattice is a poset, thus relations defined over
    -- a poset are reflexive implies that bottom `subset` bottom holds