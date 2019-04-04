module Interfaces.CompleteLattice where

--------------------------------------------------------------------------------
--                               Complete Lattice
--------------------------------------------------------------------------------

-- but man, uncomparable elements are.... UNcomparable!
class Eq p => CompleteLattice p where

    subset :: p -> p -> Bool -- not comparable elements return False

    top :: p
    top = error "[CompleteLattice] Top is not needed"

    bottom :: p

    join :: p -> p -> p

    meet :: p -> p -> p

    widen :: p -> p -> p

    narrow :: p -> p -> p

    -- auxiliary functions

    isBottom :: p -> Bool
    isBottom v = v `subset` bottom