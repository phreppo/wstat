{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Domains.SignDomain where

import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.AbstractDomain
import Interfaces.State
import Semantic.Atomic
import Semantic.AbstractEvaluation
import SyntacticStructure.WhileGrammar
import Tools.Utilities

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
                deriving (Read, Eq, Ord, Enum)

instance Show SignDomain where
    show BottomSign = bottomString
    show NonZero = "≠ 0"
    show EqualZero = "= 0"
    show GreaterZero = "> 0"
    show GreaterEqZero = "≥ 0"
    show LowerZero = "< 0"
    show LowerEqZero = "≤ 0"
    show TopSign = "⊤ "

-- SignDomain is a Complete Lattice
instance AbstractDomain SignDomain where

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

    narrow = meet

-- SignDomain is an Abstract Value Domain
instance AbstractValueDomain SignDomain where

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

    binary _        _               BottomSign      = BottomSign
    binary _        BottomSign      _               = BottomSign

    binary Add      TopSign         _               = TopSign
    binary Add      _               TopSign         = TopSign
    binary Subtract TopSign         _               = TopSign
    binary Subtract _               TopSign         = TopSign

    binary Add      LowerZero       LowerZero       = LowerZero
    binary Add      LowerZero       EqualZero       = LowerZero
    binary Add      LowerZero       LowerEqZero     = LowerZero
    binary Add      EqualZero       LowerZero       = LowerZero
    binary Add      EqualZero       EqualZero       = EqualZero
    binary Add      EqualZero       GreaterZero     = GreaterZero
    binary Add      EqualZero       LowerEqZero     = LowerEqZero
    binary Add      EqualZero       NonZero         = NonZero
    binary Add      EqualZero       GreaterEqZero   = GreaterEqZero
    binary Add      GreaterZero     EqualZero       = GreaterZero
    binary Add      GreaterZero     GreaterZero     = GreaterZero
    binary Add      GreaterZero     GreaterEqZero   = GreaterZero
    binary Add      LowerEqZero     LowerZero       = LowerZero
    binary Add      LowerEqZero     EqualZero       = LowerEqZero
    binary Add      LowerEqZero     LowerEqZero     = LowerEqZero
    binary Add      NonZero         EqualZero       = NonZero
    binary Add      GreaterEqZero   EqualZero       = GreaterEqZero
    binary Add      GreaterEqZero   GreaterZero     = GreaterZero
    binary Add      GreaterEqZero   GreaterEqZero   = GreaterEqZero

    binary Subtract LowerZero       EqualZero       = LowerZero
    binary Subtract LowerZero       GreaterZero     = LowerZero
    binary Subtract LowerZero       GreaterEqZero   = LowerZero
    binary Subtract EqualZero       LowerZero       = GreaterZero
    binary Subtract EqualZero       EqualZero       = EqualZero
    binary Subtract EqualZero       GreaterZero     = LowerZero
    binary Subtract EqualZero       LowerEqZero     = GreaterEqZero
    binary Subtract EqualZero       NonZero         = NonZero
    binary Subtract EqualZero       GreaterEqZero   = LowerEqZero
    binary Subtract GreaterZero     LowerZero       = GreaterZero
    binary Subtract GreaterZero     EqualZero       = GreaterZero
    binary Subtract GreaterZero     LowerEqZero     = GreaterZero
    binary Subtract LowerEqZero     EqualZero       = LowerEqZero
    binary Subtract LowerEqZero     GreaterZero     = LowerZero
    binary Subtract LowerEqZero     GreaterEqZero   = LowerEqZero
    binary Subtract NonZero         EqualZero       = NonZero
    binary Subtract GreaterEqZero   LowerZero       = GreaterZero
    binary Subtract GreaterEqZero   EqualZero       = GreaterEqZero
    binary Subtract GreaterEqZero   LowerEqZero     = GreaterEqZero

    binary Multiply EqualZero       _               = EqualZero
    binary Multiply _               EqualZero       = EqualZero

    binary Multiply LowerZero       LowerZero       = GreaterZero
    binary Multiply LowerZero       GreaterZero     = LowerZero
    binary Multiply LowerZero       LowerEqZero     = LowerEqZero
    binary Multiply LowerZero       NonZero         = NonZero
    binary Multiply LowerZero       GreaterEqZero   = LowerEqZero
    binary Multiply GreaterZero     LowerZero       = LowerZero
    binary Multiply GreaterZero     GreaterZero     = GreaterZero
    binary Multiply GreaterZero     LowerEqZero     = LowerEqZero
    binary Multiply GreaterZero     NonZero         = NonZero
    binary Multiply GreaterZero     GreaterEqZero   = GreaterEqZero
    binary Multiply LowerEqZero     LowerZero       = LowerEqZero
    binary Multiply LowerEqZero     GreaterZero     = LowerEqZero
    binary Multiply LowerEqZero     LowerEqZero     = GreaterEqZero
    binary Multiply LowerEqZero     GreaterEqZero   = LowerEqZero
    binary Multiply NonZero         LowerZero       = NonZero
    binary Multiply NonZero         GreaterZero     = NonZero
    binary Multiply NonZero         NonZero         = NonZero
    binary Multiply GreaterEqZero   LowerZero       = LowerEqZero
    binary Multiply GreaterEqZero   GreaterZero     = GreaterEqZero
    binary Multiply GreaterEqZero   LowerEqZero     = LowerEqZero
    binary Multiply GreaterEqZero   GreaterEqZero   = GreaterEqZero

    binary Division _               EqualZero       = BottomSign
    binary Division EqualZero       _               = EqualZero

    binary Division LowerZero       LowerZero       = GreaterZero
    binary Division LowerZero       GreaterZero     = LowerZero
    binary Division LowerZero       LowerEqZero     = GreaterZero
    binary Division LowerZero       NonZero         = NonZero
    binary Division LowerZero       GreaterEqZero   = LowerZero

    binary Division GreaterZero     LowerZero       = LowerZero
    binary Division GreaterZero     GreaterZero     = GreaterZero
    binary Division GreaterZero     LowerEqZero     = LowerZero
    binary Division GreaterZero     NonZero         = NonZero
    binary Division GreaterZero     GreaterEqZero   = GreaterZero

    binary Division LowerEqZero     LowerZero       = GreaterEqZero
    binary Division LowerEqZero     GreaterZero     = LowerEqZero
    binary Division LowerEqZero     LowerEqZero     = GreaterEqZero
    binary Division LowerEqZero     GreaterEqZero   = LowerEqZero

    binary Division NonZero         LowerZero       = NonZero
    binary Division NonZero         GreaterZero     = NonZero
    binary Division NonZero         LowerEqZero     = NonZero
    binary Division NonZero         NonZero         = NonZero
    binary Division NonZero         GreaterEqZero   = NonZero

    binary Division GreaterEqZero   LowerZero       = LowerEqZero
    binary Division GreaterEqZero   GreaterZero     = GreaterEqZero
    binary Division GreaterEqZero   LowerEqZero     = LowerEqZero
    binary Division GreaterEqZero   GreaterEqZero   = GreaterEqZero

    binary Multiply TopSign         _               = TopSign
    binary Multiply _               TopSign         = TopSign
    binary Division TopSign         _               = TopSign
    binary Division _               TopSign         = TopSign

    binary _        _               _               = TopSign

instance AbstractStateDomain SignStateDomain where
    -- assign :: AtomicAssign -> NonRelationalStateDomain b -> NonRelationalStateDomain b
    assign _ Bottom                  = Bottom
    assign (AtomicAssign var exp) x
        | isBottom $ abstractEval exp x = Bottom
        | otherwise                     = update var (abstractEval exp x) x

    -- cond :: AtomicCond -> NonRelationalStateDomain b -> NonRelationalStateDomain b
    -- all the cond match the pattern: Var {>, <, >=, <=} constant
    cond _ Bottom = Bottom
    -- cond (AtomicCond IsEqual (Var var) (IntConst number)) x
    --     | number == 0 = update var EqualZero x
    --     | number <  0 = case abstractEval (Var var) x of
    --         LowerEqZero -> x
    --         _           -> Bottom
    --     | otherwise   = case abstractEval (Var var) x of
    --         GreaterEqZero -> x
    --         _           -> Bottom

    -- cond (AtomicCond IsNEqual (Var var) (IntConst number)) x
    --     | number == 0 = case abstractEval (Var var) x of
    --         EqualZero -> Bottom
    --         _         -> x
    --     | otherwise   = x

    -- cond (AtomicCond LessEq (Var var) (IntConst number)) x
    --     | number <  0 = case abstractEval (Var var) x of
    --         LowerEqZero -> x
    --         _           -> Bottom
    --     | otherwise   = x

    -- TODO: to check this
    cond (AtomicCond GreaterEq (Var var) (IntConst number)) x
        | number >= 1  = case abstractEval (Var var) x of -- x >= [1, +inf]
            GreaterEqZero -> x
            GreaterZero -> x
            LowerEqZero  -> update var LowerZero x
            LowerZero    -> x
            NonZero      -> update var LowerZero x
            _           -> Bottom
        | number == 0 = case abstractEval (Var var) x of
            EqualZero     -> x
            LowerEqZero   -> update var EqualZero x
            GreaterEqZero -> update var EqualZero x
        | otherwise   = x -- x >= [-inf, -1]

    cond (AtomicCond Less (Var var) (IntConst number)) x
        | number <= 0  = case abstractEval (Var var) x of -- x < [-inf, 0]
            LowerEqZero   -> update var LowerZero x
            LowerZero     -> x
            NonZero       -> update var LowerZero x
            _             -> Bottom
        | number == 1 = case abstractEval (Var var) x of -- x < 1
            EqualZero     -> x
            LowerEqZero   -> x
            LowerZero     -> x
            NonZero       -> update var LowerZero x
            GreaterEqZero -> update var EqualZero x
            _             -> Bottom
        | otherwise        = x -- x < [2, +inf]

    -- cond (AtomicCond Greater (Var var) (IntConst number)) x
    --     | number >= 0 = case abstractEval (Var var) x of
    --         GreaterEqZero -> x
    --         _           -> Bottom
    --     | otherwise   = x

    cond (AtomicCond _ _ _) x = x -- always a sound abstraction

type SignStateDomain = NonRelationalStateDomain Var SignDomain
