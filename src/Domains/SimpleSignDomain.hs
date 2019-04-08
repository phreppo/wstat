{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Domains.SimpleSignDomain where

import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import Interfaces.State
import Semantic.Atomic
import Semantic.Evaluation
import SyntacticStructure.WhileGrammar
import Tools.Utilities

--------------------------------------------------------------------------------
--                             Sign Domain
--------------------------------------------------------------------------------

data SimpleSignDomain = BottomSign
                      | EqualZero
                      | GreaterEqZero
                      | LowerEqZero
                      | TopSign
                      deriving (Read, Eq, Ord, Enum)

instance Show SimpleSignDomain where
    show BottomSign = bottomString
    show EqualZero = "= 0"
    show GreaterEqZero = "≥ 0"
    show LowerEqZero = "≤ 0"
    show TopSign = "⊤ "

-- SimpleSignDomain is a Complete Lattice
instance CompleteLattice SimpleSignDomain where

    subset = (<=) -- auto inferred from deriving Ord

    top = TopSign

    bottom = BottomSign

    join TopSign            _               = TopSign
    join _                  TopSign         = TopSign
    join BottomSign         x               = x
    join x                  BottomSign      = x
    join EqualZero          x               = x -- x can't be BottomSign
    join x                  EqualZero       = x -- x can't be BottomSign
    join GreaterEqZero      LowerEqZero     = TopSign
    join LowerEqZero        GreaterEqZero   = TopSign
    join x                  _               = x

    meet BottomSign         _               = BottomSign
    meet _                  BottomSign      = BottomSign
    meet TopSign            x               = x
    meet x                  TopSign         = x
    meet EqualZero          x               = EqualZero
    meet x                  EqualZero       = EqualZero
    meet GreaterEqZero      LowerEqZero     = EqualZero
    meet LowerEqZero        GreaterEqZero   = EqualZero
    meet x                  _               = x

    widen = join -- the Domain isn't infinite: no need of widening
    
    narrow = meet

-- SimpleSignDomain is an Abstract Value Domain
instance AbstractValueDomain SimpleSignDomain where

    cons x | x == 0    = EqualZero
           | x >= 0    = GreaterEqZero
           | otherwise = LowerEqZero

    rand NegInf (Negative _) = LowerEqZero
    rand NegInf (Positive 0) = LowerEqZero
    rand NegInf (Positive _) = TopSign

    rand (Positive _) PosInf = GreaterEqZero
    rand (Negative 0) PosInf = GreaterEqZero
    rand (Negative _) PosInf = TopSign

    rand (Positive 0) (Positive 0) = EqualZero
    rand (Negative 0) (Negative 0) = EqualZero

    rand (Positive _) (Positive _) = GreaterEqZero
    rand (Negative _) (Negative _) = LowerEqZero

    rand _ _ = TopSign

    unary Neg GreaterEqZero = LowerEqZero
    unary Neg LowerEqZero   = GreaterEqZero
    unary Neg x             = x

    binary _        _               BottomSign      = BottomSign
    binary _        BottomSign      _               = BottomSign

    binary Add      TopSign         _               = TopSign
    binary Add      _               TopSign         = TopSign
    binary Add      GreaterEqZero   GreaterEqZero   = GreaterEqZero
    binary Add      LowerEqZero     GreaterEqZero   = TopSign
    binary Add      EqualZero       GreaterEqZero   = GreaterEqZero
    binary Add      GreaterEqZero   LowerEqZero     = TopSign
    binary Add      LowerEqZero     LowerEqZero     = LowerEqZero
    binary Add      EqualZero       LowerEqZero     = LowerEqZero
    binary Add      GreaterEqZero   EqualZero       = GreaterEqZero
    binary Add      LowerEqZero     EqualZero       = LowerEqZero
    binary Add      EqualZero       EqualZero       = EqualZero

    binary Subtract TopSign         _               = TopSign
    binary Subtract _               TopSign         = TopSign
    binary Subtract GreaterEqZero   GreaterEqZero   = TopSign
    binary Subtract LowerEqZero     GreaterEqZero   = GreaterEqZero
    binary Subtract EqualZero       GreaterEqZero   = GreaterEqZero
    binary Subtract GreaterEqZero   LowerEqZero     = LowerEqZero
    binary Subtract LowerEqZero     LowerEqZero     = TopSign
    binary Subtract EqualZero       LowerEqZero     = LowerEqZero
    binary Subtract GreaterEqZero   EqualZero       = LowerEqZero
    binary Subtract LowerEqZero     EqualZero       = GreaterEqZero
    binary Subtract EqualZero       EqualZero       = EqualZero

    binary Multiply GreaterEqZero   GreaterEqZero   = GreaterEqZero
    binary Multiply LowerEqZero     GreaterEqZero   = LowerEqZero
    binary Multiply EqualZero       GreaterEqZero   = EqualZero
    binary Multiply GreaterEqZero   LowerEqZero     = LowerEqZero
    binary Multiply LowerEqZero     LowerEqZero     = GreaterEqZero
    binary Multiply EqualZero       LowerEqZero     = EqualZero
    binary Multiply GreaterEqZero   EqualZero       = EqualZero
    binary Multiply LowerEqZero     EqualZero       = EqualZero
    binary Multiply EqualZero       EqualZero       = EqualZero
    binary Multiply EqualZero       TopSign         = EqualZero
    binary Multiply TopSign         EqualZero       = EqualZero
    binary Multiply TopSign         _               = TopSign
    binary Multiply _               TopSign         = TopSign

    binary Division GreaterEqZero   GreaterEqZero   = GreaterEqZero
    binary Division LowerEqZero     GreaterEqZero   = LowerEqZero
    binary Division EqualZero       GreaterEqZero   = EqualZero
    binary Division GreaterEqZero   LowerEqZero     = LowerEqZero
    binary Division LowerEqZero     LowerEqZero     = GreaterEqZero
    binary Division EqualZero       LowerEqZero     = EqualZero
    binary Division GreaterEqZero   EqualZero       = BottomSign
    binary Division LowerEqZero     EqualZero       = BottomSign
    binary Division EqualZero       EqualZero       = BottomSign
    binary Division TopSign         EqualZero       = BottomSign
    binary Division EqualZero       TopSign         = EqualZero
    binary Division TopSign         _               = TopSign
    binary Division _               TopSign         = TopSign

instance AbstractStateDomain (NonRelationalStateDomain Var SimpleSignDomain) where
    -- assign :: AtomicAssign -> SimpleSignStateDomain -> SimpleSignStateDomain
    assign _ Bottom                  = Bottom
    assign (AtomicAssign var exp) x
        | isBottom $ abstractEval exp x = Bottom
        | otherwise                     = update var (abstractEval exp x) x

    -- cond :: AtomicCond -> SimpleSignStateDomain -> SimpleSignStateDomain
    cond _ Bottom = Bottom
    cond (AtomicCond IsEqual (Var var) (IntConst number)) x
        | number == 0 = update var EqualZero x
        | number <  0 = case abstractEval (Var var) x of
            LowerEqZero -> x
            _           -> Bottom
        | otherwise   = case abstractEval (Var var) x of
            GreaterEqZero -> x
            _           -> Bottom

    cond (AtomicCond IsNEqual (Var var) (IntConst number)) x
        | number == 0 = case abstractEval (Var var) x of
            EqualZero -> Bottom
            _         -> x
        | otherwise   = x

    cond (AtomicCond LessEq (Var var) (IntConst number)) x
        | number <  0 = case abstractEval (Var var) x of
            LowerEqZero -> x
            _           -> Bottom
        | otherwise   = x

    cond (AtomicCond GreaterEq (Var var) (IntConst number)) x
        | number >  0 = case abstractEval (Var var) x of
            GreaterEqZero -> x
            _             -> Bottom
        | otherwise   = x

    cond (AtomicCond Less (Var var) (IntConst number)) x
        | number <= 0 = case abstractEval (Var var) x of
            LowerEqZero -> x
            _           -> Bottom
        | otherwise   = x

    cond (AtomicCond Greater (Var var) (IntConst number)) x
        | number >= 0 = case abstractEval (Var var) x of
            GreaterEqZero -> x
            _           -> Bottom
        | otherwise   = x

    cond (AtomicCond _ _ _) x = x -- always a sound abstraction

type SimpleSignStateDomain = NonRelationalStateDomain Var SimpleSignDomain