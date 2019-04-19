{-# LANGUAGE MultiParamTypeClasses #-}

module Domains.ReductionIntervalCongruenceDomain where

--

import Domains.CongruenceDomain
import Domains.IntervalDomain
import Interfaces.ReducedProductDomain
import Tools.Utilities
import SyntacticStructure.WhileGrammar
import Interfaces.AbstractStateDomain

instance ReducedProductDomain IntervalDomain CongruenceDomain where

    reduction (BottomInterval, x) = (BottomInterval, x)
    reduction (x, BottomCongruence) = (x, BottomCongruence)
    reduction (Interval a b, Congruence c d)
            | a' > b'   = (BottomInterval, BottomCongruence)
            | a' == b'  = let N aInt = a' in (Interval a' a', Congruence 0 aInt)
            | otherwise = (Interval a' b', Congruence c d)
        where
            a' = case a of
                N aInt -> let Just result = filterFirst (\x -> isCongruence x d c) [aInt..] in N result
                NegativeInf -> NegativeInf
            b' = case b of
                N bInt -> let Just result = filterFirst (\x -> isCongruence x d c) (reverse [0..bInt]) in N result
                PositiveInf -> PositiveInf


type ReductionIntervalCongruenceStateDomain = NonRelationalStateDomain Var (ReducedProduct IntervalDomain CongruenceDomain)