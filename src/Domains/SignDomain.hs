{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Domains.SignDomain where

import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import Interfaces.State
import Semantic.Atomic
import Semantic.Evaluation
import SyntacticStructure.WhileGrammar

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

    subset BottomSign  _             = True
    subset _           BottomSign    = False

    subset _           TopSign       = True
    subset TopSign     _             = False

    subset EqualZero   LowerEqZero   = True
    subset EqualZero   GreaterEqZero = True
    subset EqualZero   EqualZero     = True
    subset EqualZero   _             = False
    subset _           EqualZero     = False

    subset LowerZero   LowerEqZero   = True
    subset LowerZero   NonZero       = True
    subset LowerZero   LowerZero     = True
    subset LowerZero   _             = False
    subset _           LowerZero     = False

    subset GreaterZero GreaterEqZero = True
    subset GreaterZero NonZero       = True
    subset GreaterZero GreaterZero   = True
    subset GreaterZero _             = False
    subset _           GreaterZero   = False

    subset x           y             = x == y

    top    = TopSign
    bottom = BottomSign

    join TopSign        _               = TopSign
    join _              TopSign         = TopSign

    join BottomSign     x               = x
    join x              BottomSign      = x

    join NonZero        NonZero         = NonZero
    join EqualZero      EqualZero       = EqualZero
    join GreaterZero    GreaterZero     = GreaterZero
    join GreaterEqZero  GreaterEqZero   = GreaterEqZero
    join LowerZero      LowerZero       = LowerZero
    join LowerEqZero    LowerEqZero     = LowerEqZero

    join NonZero        LowerZero       = NonZero
    join NonZero        GreaterZero     = NonZero

    join LowerEqZero    LowerZero       = LowerEqZero
    join LowerEqZero    EqualZero       = LowerEqZero

    join GreaterEqZero  GreaterZero     = GreaterEqZero
    join GreaterEqZero  EqualZero       = GreaterEqZero

    join EqualZero      LowerEqZero     = LowerEqZero
    join EqualZero      GreaterEqZero   = GreaterEqZero
    join EqualZero      LowerZero       = LowerEqZero
    join EqualZero      GreaterZero     = GreaterEqZero

    join LowerZero      LowerEqZero     = LowerEqZero
    join LowerZero      NonZero         = NonZero
    join LowerZero      EqualZero       = LowerEqZero
    join LowerZero      GreaterZero     = NonZero

    join GreaterZero    GreaterEqZero   = GreaterEqZero
    join GreaterZero    NonZero         = NonZero
    join GreaterZero    EqualZero       = GreaterEqZero
    join GreaterZero    LowerZero       = NonZero

    join _              _               = TopSign

    meet BottomSign     _               = BottomSign
    meet _              BottomSign      = BottomSign

    meet TopSign        x               = x
    meet x              TopSign         = x

    meet NonZero        NonZero         = NonZero
    meet EqualZero      EqualZero       = EqualZero
    meet GreaterZero    GreaterZero     = GreaterZero
    meet GreaterEqZero  GreaterEqZero   = GreaterEqZero
    meet LowerZero      LowerZero       = LowerZero
    meet LowerEqZero    LowerEqZero     = LowerEqZero

    meet NonZero        LowerZero       = LowerZero
    meet NonZero        GreaterZero     = GreaterZero
    meet NonZero        LowerEqZero     = LowerZero
    meet NonZero        GreaterEqZero   = GreaterZero

    meet LowerEqZero    LowerZero       = LowerZero
    meet LowerEqZero    EqualZero       = EqualZero
    meet LowerEqZero    NonZero         = LowerZero
    meet LowerEqZero    GreaterEqZero   = EqualZero

    meet GreaterEqZero  GreaterZero     = GreaterZero
    meet GreaterEqZero  EqualZero       = EqualZero
    meet GreaterEqZero  NonZero         = GreaterZero
    meet GreaterEqZero  LowerEqZero     = EqualZero

    meet EqualZero      LowerEqZero     = EqualZero
    meet EqualZero      GreaterEqZero   = EqualZero

    meet LowerZero      LowerEqZero     = LowerZero
    meet LowerZero      NonZero         = LowerZero

    meet GreaterZero    GreaterEqZero   = GreaterZero
    meet GreaterZero    NonZero         = GreaterZero

    meet _              _               = BottomSign

    widen = join

-- SignDomain is an Abstract Value Domain
instance AVD SignDomain where

    cons x | x == 0    = EqualZero
           | x > 0     = GreaterZero
           | otherwise = LowerZero

    rand NegInf (Negative 0) = LowerEqZero
    rand NegInf (Negative _) = LowerZero
    rand NegInf (Positive 0) = LowerEqZero
    rand NegInf (Positive _) = TopSign

    rand (Positive 0) PosInf = GreaterEqZero
    rand (Positive _) PosInf = GreaterZero
    rand (Negative 0) PosInf = GreaterEqZero
    rand (Negative _) PosInf = TopSign

    rand (Positive 0) (Positive 0) = EqualZero
    rand (Negative 0) (Negative 0) = EqualZero

    rand (Positive 0) (Positive _) = GreaterEqZero
    rand (Positive _) (Positive _) = GreaterZero
    rand (Negative _) (Negative 0) = LowerEqZero
    rand (Negative _) (Negative _) = LowerZero

    rand _ _ = TopSign

    unary Neg NonZero       = EqualZero
    unary Neg EqualZero     = NonZero
    unary Neg GreaterZero   = LowerZero
    unary Neg LowerZero     = GreaterZero
    unary Neg GreaterEqZero = LowerEqZero
    unary Neg LowerEqZero   = GreaterEqZero
    unary Neg x             = x

instance ASD SignStateDomain where
    -- assign :: AtomicAssign -> SD b -> SD b
    assign _ Bottom                  = Bottom
    assign (AtomicAssign var exp) x
        | isBottom $ abstractEval exp x = Bottom
        | otherwise                     = update var (abstractEval exp x) x


type SignStateDomain = SD Var SignDomain
