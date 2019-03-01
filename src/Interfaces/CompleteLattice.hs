module Interfaces.CompleteLattice where

-- minimal interface for a Complete Lattice

class CompleteLattice p where

    subset :: p -> p -> Bool -- non comparable elements returns False

    top :: p

    bottom :: p

    join :: p -> p -> p

    meet :: p -> p -> p

    widen :: p -> p -> p

    -- usefull auxiliary functions

    isBottom :: p -> Bool
    isBottom v = v `subset` bottom