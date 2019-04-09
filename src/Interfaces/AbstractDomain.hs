module Interfaces.AbstractDomain where

-- we have grouped all these operations in a single class due to haskell
-- implementation's mechanism,
-- if we modular into multiple class this 'AbstractDomain' module, then
-- for any both concrete and abstract domains we have to define each multiple
-- instances,
-- in conclusion the former saved a lot of work

class Eq p => AbstractDomain p where

--------------------------------------------------------------------------------
--                                    Poset
--------------------------------------------------------------------------------

    subset :: p -> p -> Bool

--------------------------------------------------------------------------------
--                                   Lattice
--------------------------------------------------------------------------------

    join :: p -> p -> p

    meet :: p -> p -> p

--------------------------------------------------------------------------------
--                               Complete Lattice
--------------------------------------------------------------------------------

    top :: p

    bottom :: p

--------------------------------------------------------------------------------
--                               Abstract Domain
--------------------------------------------------------------------------------

    widen :: p -> p -> p

    narrow :: p -> p -> p

    -- auxiliary functions

    isBottom :: p -> Bool
    isBottom v = v `subset` bottom
    -- complete lattice is a poset, thus relations defined over
    -- a poset are reflexive implies that bottom `subset` bottom holds