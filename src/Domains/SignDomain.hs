{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Domains.SignDomain where

import Interfaces.CompleteLattice

--------------------------------------------------------------------------------
--                             Sign Domain
--------------------------------------------------------------------------------

data SignDomain = BottomSign
                | NonZero
                | EqualZero
                | GreaterZero
                | GreaterEqZero
                | LowerZero
                | LowerEqZero
                | TopSign
                deriving (Show, Read, Eq, Ord, Enum)

-- SignDomain is a Complete Lattice
instance CompleteLattice SignDomain where

 -- subset GreaterEqZero    GreaterEqZero   = True
    subset BottomSign       _               = True
    subset _                BottomSign      = False
    subset _                TopSign         = True
    subset TopSign          _               = False

    subset EqualZero        LowerEqZero     = True
    subset EqualZero        GreaterEqZero   = True
    subset EqualZero        EqualZero       = True
    subset EqualZero        _               = False
    subset _                EqualZero       = False

    subset LowerZero        LowerEqZero     = True
    subset LowerZero        NonZero         = True
    subset LowerZero        LowerZero       = True
    subset LowerZero        _               = False
    subset _                LowerZero       = False

    subset GreaterZero      GreaterEqZero   = True
    subset GreaterZero      NonZero         = True
    subset GreaterZero      GreaterZero     = True
    subset GreaterZero      _               = False
    subset _                GreaterZero     = False

    subset x                y               = x == y

