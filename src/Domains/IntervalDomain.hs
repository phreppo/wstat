{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Domains.IntervalDomain where

import Data.Functor
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.AbstractDomain
import Interfaces.State
import Semantic.Atomic
import Semantic.AbstractSematic
import Semantic.AbstractEvaluation
import SyntacticStructure.WhileGrammar
import Tools.Utilities


--------------------------------------------------------------------------------
--                             Interval Domain
--------------------------------------------------------------------------------

data IntervalValue = NegativeInf
                   | N I
                   | PositiveInf
                   deriving (Read, Eq, Ord)

instance Show IntervalValue where
    show PositiveInf = "+∞"
    show NegativeInf = "-∞"
    show (N x) = show x



data IntervalDomain = Interval IntervalValue IntervalValue
                    | BottomInterval
                    deriving (Read, Eq, Ord)

instance Show IntervalDomain where
    show BottomInterval = bottomString
    show (Interval NegativeInf PositiveInf) = "∊ ⊤"
    show (Interval a b) = "∊ [" ++ show a ++ ", " ++ show b ++ "]"

instance AbstractDomain IntervalDomain where

    subset BottomInterval _ = True
    subset _ BottomInterval = False
    subset (Interval a b) (Interval c d) = (a >= c) && (b <= d)

    top = Interval NegativeInf PositiveInf

    bottom = BottomInterval

    join BottomInterval x = x -- optimal
    join x BottomInterval = x
    join (Interval a b) (Interval c d) = Interval (min a c) (max b d)

    meet BottomInterval _ = BottomInterval -- exact
    meet _ BottomInterval = BottomInterval
    meet (Interval a b) (Interval c d)
        | (max a c) <= (min b d) = Interval (max a c) (min b d)
        | otherwise = BottomInterval

    widen BottomInterval x = x
    widen x BottomInterval = x
    widen (Interval a b) (Interval c d) = (Interval leftBound rightBound)
        where leftBound  = if a <= c then a else NegativeInf
              rightBound = if b >= d then b else PositiveInf

    narrow BottomInterval x = x
    narrow x BottomInterval = x
    narrow (Interval a b) (Interval c d) = (Interval leftBound rightBound)
        where leftBound  = if a == NegativeInf then c else a
              rightBound = if b == PositiveInf then d else b

instance AbstractValueDomain IntervalDomain where

    cons c     = Interval (N c) (N c)                                               -- exact

    rand c1 c2 = Interval (convertToIntervalNumber c1) (convertToIntervalNumber c2) -- exact

    unary Neg BottomInterval = BottomInterval                                       -- exact
    unary Neg (Interval a b) = Interval (invert b) (invert a)

    binary _ BottomInterval _ = BottomInterval
    binary _ _ BottomInterval = BottomInterval
    binary Add      (Interval a b) (Interval c d) = addIntervals (a,b) (c,d)        -- exact
    binary Subtract (Interval a b) (Interval c d) = subtractIntervals (a,b) (c,d)   -- exact
    binary Multiply (Interval a b) (Interval c d) = multiplyIntervals (a,b) (c,d)   -- optimal
    binary Division (Interval a b) (Interval c d) = divideIntervals (a,b) (c,d)     -- optimal

instance AbstractStateDomain IntervalStateDomain where
    -- cond :: AtomicCond -> IntervalStateDomain -> IntervalStateDomain
    cond _ Bottom = Bottom

    cond (AtomicCond operator (Var var) (IntConst v)) state = -- var ⋈ const
        condCompareVarConst operator var v state

    cond (AtomicCond operator (IntConst v) (Var var)) state = -- const ⋈ var
        condCompareVarConst (negateRelationalOperator operator) var v state

    cond (AtomicCond operator (Var var1) (Var var2)) state = -- var ⋈ var
        condCompareVarVar operator var1 var2 state

    cond (AtomicCond operator left right) state = state -- always sound

    -- assign :: AtomicAssign -> IntervalStateDomain -> IntervalStateDomain
    assign _ Bottom                  = Bottom
    assign (AtomicAssign var exp) state
        | isBottom $ valuedExpression = Bottom -- smashed bottom
        | otherwise                   = update var valuedExpression state
        where valuedExpression = abstractEval exp state


type IntervalStateDomain = NonRelationalStateDomain Var IntervalDomain


convertToIntervalNumber :: SignedInfiniteInteger -> IntervalValue
convertToIntervalNumber (Positive x) = N x
convertToIntervalNumber (Negative x) = N (-x)
convertToIntervalNumber PosInf = PositiveInf
convertToIntervalNumber NegInf = NegativeInf

invert :: IntervalValue -> IntervalValue
invert PositiveInf = NegativeInf
invert NegativeInf = PositiveInf
invert (N x) = N (-x)

addIntervals :: (IntervalValue, IntervalValue) -> (IntervalValue, IntervalValue) -> IntervalDomain
addIntervals (a, b) (c, d) = Interval (addIntervalValues a c) (addIntervalValues b d)

addIntervalValues :: IntervalValue -> IntervalValue -> IntervalValue
addIntervalValues PositiveInf NegativeInf = error "added neginf to posinf"
addIntervalValues NegativeInf PositiveInf = error "added posinf to neginf"
addIntervalValues PositiveInf _           = PositiveInf
addIntervalValues _ PositiveInf           = PositiveInf
addIntervalValues NegativeInf _           = NegativeInf
addIntervalValues _ NegativeInf           = NegativeInf
addIntervalValues (N x) (N y)             = N (x + y)

subtractIntervals :: (IntervalValue, IntervalValue) -> (IntervalValue, IntervalValue) -> IntervalDomain
subtractIntervals (a, b) (c, d) = Interval (subIntervalValues a d) (subIntervalValues b c)

subIntervalValues :: IntervalValue -> IntervalValue -> IntervalValue
subIntervalValues PositiveInf NegativeInf = error "subtracted neginf to posinf"
subIntervalValues NegativeInf PositiveInf = error "subtracted posinf to neginf"
subIntervalValues PositiveInf _           = PositiveInf
subIntervalValues _ PositiveInf           = NegativeInf
subIntervalValues NegativeInf _           = NegativeInf
subIntervalValues _ NegativeInf           = PositiveInf
subIntervalValues (N x) (N y)             = N (x - y)

multiplyIntervals :: (IntervalValue, IntervalValue) -> (IntervalValue, IntervalValue) -> IntervalDomain
multiplyIntervals (a, b) (c, d) = Interval (minimum [ac, ad, bc, bd]) (maximum [ac, ad, bc, bd])
    where ac = multIntervalValues a c
          ad = multIntervalValues a d
          bc = multIntervalValues b c
          bd = multIntervalValues b d

multIntervalValues :: IntervalValue -> IntervalValue -> IntervalValue
multIntervalValues PositiveInf NegativeInf = NegativeInf
multIntervalValues NegativeInf PositiveInf = NegativeInf
multIntervalValues PositiveInf PositiveInf = PositiveInf
multIntervalValues NegativeInf NegativeInf = PositiveInf

multIntervalValues PositiveInf (N 0)       = N 0 -- non standard, described in the notes: [0, 0] * [1, +inf] = [0, 0]
multIntervalValues (N 0) PositiveInf       = N 0
multIntervalValues PositiveInf (N x)       = if x > 0 then PositiveInf else NegativeInf
multIntervalValues (N x) PositiveInf       = if x > 0 then PositiveInf else NegativeInf

multIntervalValues NegativeInf (N 0)       = N 0 -- non standard
multIntervalValues (N 0) NegativeInf       = N 0
multIntervalValues NegativeInf (N x)       = if x > 0 then NegativeInf else PositiveInf
multIntervalValues (N x) NegativeInf       = if x > 0 then NegativeInf else PositiveInf

multIntervalValues (N x) (N y)             = N (x * y)

divideIntervals :: (IntervalValue, IntervalValue) -> (IntervalValue, IntervalValue) -> IntervalDomain
divideIntervals _ (N 0, N 0)  = BottomInterval
divideIntervals (a, b) (c, d) | (N 0) <= c = -- positive interval
                                    let afc = divideIntervalValues a c -- a fract d
                                        afd = divideIntervalValues a d -- div for d case to handle reals, not needed with int
                                        bfc = divideIntervalValues b c
                                        bfd = divideIntervalValues b d in
                                    Interval (minimum [afc, afd, bfc, bfd]) (maximum [afc, afd, bfc, bfd])
                              | d <= (N 0) = -- negative interval
                                    divideIntervals (invert b, invert a) (invert d, invert c) -- divide and mult for (-1)
                              | otherwise  = -- mixed
                                    (divideIntervals (a, b) (c, N 0)) `join` (divideIntervals (a, b) (N 0, d))


divideIntervalValues :: IntervalValue -> IntervalValue -> IntervalValue
divideIntervalValues PositiveInf (N 0)       = PositiveInf -- can't be an error
divideIntervalValues NegativeInf (N 0)       = NegativeInf
divideIntervalValues (N 0)       (N 0)       = N 0 -- [0, 0] / [0, 1]
divideIntervalValues (N x)       (N 0)       = if x > 0 then PositiveInf else NegativeInf

divideIntervalValues PositiveInf (N x)       = if x > 0 then PositiveInf else NegativeInf
divideIntervalValues NegativeInf (N x)       = if x > 0 then NegativeInf else PositiveInf

divideIntervalValues (N 0)       PositiveInf = N 0
divideIntervalValues (N 0)       NegativeInf = N 0
divideIntervalValues (N x)       PositiveInf = N 0
divideIntervalValues (N x)       NegativeInf = N 0
divideIntervalValues PositiveInf PositiveInf = N 0 -- non-standard: for compatibility with mult: +inf/+inf = +inf / (1 / +inf)
divideIntervalValues NegativeInf NegativeInf = N 0
divideIntervalValues NegativeInf PositiveInf = N 0
divideIntervalValues PositiveInf NegativeInf = N 0

divideIntervalValues (N x)       (N y)       = N (x `div` y) -- caution to the type of the division

condCompareVarConst :: BArithmeticBinOperator -> String -> I -> IntervalStateDomain -> IntervalStateDomain
condCompareVarConst LessEq var v state = -- var <= const
    case abstractEvalVar var state of
        BottomInterval -> Bottom -- smashed bottom
        Interval a b   ->
            if a <= (N v)
                then update var (Interval a (min b (N v)) ) state
                else Bottom -- a > v
condCompareVarConst Less var v state = -- var < const
    case abstractEvalVar var state of
        BottomInterval -> Bottom
        Interval a b   ->
            if a < (N v)
                then update var (Interval a (min b (N $ v-1)) ) state
                else Bottom -- a >= v
condCompareVarConst GreaterEq var v state = -- var >= const
    case abstractEvalVar var state of
        BottomInterval -> Bottom
        Interval a b   ->
            if b >= (N v)
                then update var (Interval (max a (N v)) b ) state
                else Bottom -- b < v
condCompareVarConst Greater var v state = -- var > const
    case abstractEvalVar var state of
        BottomInterval -> Bottom
        Interval a b   ->
            if b > (N v)
                then update var (Interval (max a (N $ v+1)) b ) state
                else Bottom -- b <= v
condCompareVarConst IsEqual var v state = -- var = const
    case abstractEvalVar var state of
        BottomInterval -> Bottom
        Interval a b   ->
            if b >= (N v) && a <= (N v)
                then update var (Interval (N v) (N v)) state
                else Bottom -- v is not in [a, b]
condCompareVarConst IsNEqual var v state = -- var != const
    case abstractEvalVar var state of
        BottomInterval -> Bottom
        Interval a b   ->
            if b < (N v) || a > (N v) -- [a, b] does not intersects v
                then state
                else if a /= b then state   -- a != b && (b >= v && a <= v) => v is in the interval [a, b] and for soundness we can't go bottom
                               else Bottom  -- a == b && (b >= v && a <= v) => a = b = v

condCompareVarVar :: BArithmeticBinOperator -> String -> String -> IntervalStateDomain -> IntervalStateDomain
condCompareVarVar LessEq var1 var2 state = -- var <= var
    let evaluedVar1 = abstractEvalVar var1 state
        evaluedVar2 = abstractEvalVar var2 state in
            case evaluedVar1 of
                BottomInterval -> Bottom -- smashed bottom
                Interval a b   ->
                    case evaluedVar2 of
                        BottomInterval -> Bottom
                        Interval c d   ->
                            if a <= d
                                then (update var2 (Interval (max a c) d) .  update var1 (Interval a (min b d)) ) state
                                else Bottom -- a > d
condCompareVarVar Less var1 var2 state = -- var < var
    let evaluedVar1 = abstractEvalVar var1 state
        evaluedVar2 = abstractEvalVar var2 state in
            case evaluedVar1 of
                BottomInterval -> Bottom
                Interval a b   ->
                    case evaluedVar2 of
                        BottomInterval -> Bottom
                        Interval c d   ->
                            if a < d
                                then (update var2 (Interval (max (addIntervalValues a (N 1)) c) d) .  update var1 (Interval a (min b (subIntervalValues d (N 1)))) ) state
                                else Bottom -- a >= d
condCompareVarVar GreaterEq var1 var2 state = -- var >= var
    let evaluedVar1 = abstractEvalVar var1 state
        evaluedVar2 = abstractEvalVar var2 state in
            case evaluedVar1 of
                BottomInterval -> Bottom
                Interval a b   ->
                    case evaluedVar2 of
                        BottomInterval -> Bottom
                        Interval c d   ->
                            if b >= c
                                then (update var2 (Interval c (min b d)) .  update var1 (Interval (max a c) b) ) state
                                else Bottom -- b <= c
condCompareVarVar Greater var1 var2 state = -- var > var
    let evaluedVar1 = abstractEvalVar var1 state
        evaluedVar2 = abstractEvalVar var2 state in
            case evaluedVar1 of
                BottomInterval -> Bottom
                Interval a b   ->
                    case evaluedVar2 of
                        BottomInterval -> Bottom
                        Interval c d   ->
                            if b > c
                                then (update var2 (Interval c (min (subIntervalValues b (N 1)) d))
                                    .  update var1 (Interval (max a (addIntervalValues c (N 1))) b) ) state
                                else Bottom -- b <= c
condCompareVarVar IsEqual var1 var2 state = -- var = var
    let evaluedVar1 = abstractEvalVar var1 state
        evaluedVar2 = abstractEvalVar var2 state in
            case evaluedVar1 of
                BottomInterval -> Bottom
                int1   ->
                    case evaluedVar2 of
                        BottomInterval -> Bottom
                        int2   ->
                            case int1 `meet` int2 of
                                BottomInterval -> Bottom -- empty intersection
                                intersection -> (update var2 intersection . update var1 intersection) state
condCompareVarVar IsNEqual var1 var2 state =
    let evaluedVar1 = abstractEvalVar var1 state
        evaluedVar2 = abstractEvalVar var2 state in
            case evaluedVar1 of
                BottomInterval -> Bottom
                int1   ->
                    case evaluedVar2 of
                        BottomInterval -> Bottom
                        int2   ->
                            case int1 `meet` int2 of
                                BottomInterval -> state -- empty intersection
                                Interval a b   -> if a == b then Bottom
                                                            else state

checkIntervalState :: IntervalStateDomain -> IntervalStateDomain
checkIntervalState state = if stateContainsEmptyInterval state
                            then Bottom
                            else state

stateContainsEmptyInterval :: IntervalStateDomain -> Bool
stateContainsEmptyInterval state =
    any isEmptyInterval values
    where keys = getVars state
          values = [ Interfaces.State.lookup k state | k <- keys  ]

isEmptyInterval :: IntervalDomain -> Bool
isEmptyInterval BottomInterval = True
isEmptyInterval (Interval a b) = b < a
