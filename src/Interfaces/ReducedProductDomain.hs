{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Interfaces.ReducedProductDomain where

--
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.AbstractDomain
import Semantic.Atomic
import SyntacticStructure.WhileGrammar
import Interfaces.State as S
import Semantic.AbstractEvaluation

type ReducedProduct a b = (a, b)

instance (
    AbstractDomain a,
    AbstractDomain b,
    ReducedProductDomain a b
  ) => AbstractDomain (ReducedProduct a b) where

    (a, b) `subset` (a', b') = a `subset` a' && b `subset` b'

    bottom = (bottom, bottom)

    top = (top, top)

    (a, b) `join` (a', b') = reduction $ (a `join` a', b `join` b')

    (a, b) `meet` (a', b') = reduction $ (a `meet` a', b `meet` b')

    (a, b) `widen` (a', b') = (a `widen` a', b `widen` b')

    (a, b) `narrow` (a', b') = (a `narrow` a', b `narrow` b')

instance (
    AbstractValueDomain a,
    AbstractValueDomain b,
    ReducedProductDomain a b
  ) => AbstractValueDomain (ReducedProduct a b) where

    cons x = (cons x, cons x)

    rand x y = (rand x y, rand x y)

    unary op (x, y) = reduction (unary op x, unary op y)

    binary op (a, b) (a', b') = reduction (binary op a a', binary op b b')

type ReducedProductStateDomain a b = NonRelationalStateDomain Var (ReducedProduct a b)

instance (
    AbstractValueDomain a,
    AbstractValueDomain b,
    AbstractStateDomain (NonRelationalStateDomain Var a),
    AbstractStateDomain (NonRelationalStateDomain Var b),
    ReducedProductDomain a b
  ) => AbstractStateDomain (ReducedProductStateDomain a b) where

    assign _ Bottom = Bottom
    assign (AtomicAssign var exp) state
        | isBottom $ valuedExpFstState = Bottom -- smashed bottom
        | isBottom $ valuedExpSndState = Bottom -- smashed bottom
        | otherwise                    = update var (reduction (valuedExpFstState, valuedExpSndState)) state
        where valuedExpFstState        = abstractEval exp $ fstStateDomain state
              valuedExpSndState        = abstractEval exp $ sndStateDomain state

    cond _ Bottom = Bottom
    cond condition state
        | isBottom $ fstState = Bottom -- smashed bottom
        | isBottom $ sndState = Bottom -- smashed bottom
        | otherwise                    = mergeStateDomain fstState sndState
        where fstState                 = cond condition $ fstStateDomain state
              sndState                 = cond condition $ sndStateDomain state
            --   a = (fstStateDomain state) :: NonRelationalStateDomain Var a

-- given a product state (a, b) and the constraint that a and b are state
-- update each tuple in the product domain with the reduction of the corresponding value
-- in a and b
mergeStateDomain ::
    (ReducedProductDomain a b, State s k (ReducedProduct a b), State s k a, State s k b) =>
    s k a -> s k b -> s k (ReducedProduct a b)
mergeStateDomain fstState sndState = fromList $ fmap (\k -> (k, reduction (S.lookup k fstState, S.lookup k sndState))) $ getVars fstState

-- given a product state return the first element of each tuple in the map
fstStateDomain :: (State s k a, State s k (ReducedProduct a b)) =>
    s k (ReducedProduct a b) -> s k a
fstStateDomain state = (fromList $ fmap (\k -> let (fst, _) = S.lookup k state in (k, fst)) $ getVars state)

-- given a product state return the second element of each tuple in the map
sndStateDomain :: (State s k b, State s k (ReducedProduct a b)) => s k (ReducedProduct a b) -> s k b
sndStateDomain state = fromList $ fmap (\k -> let (_, snd) = S.lookup k state in (k, snd)) $ getVars state


class (
        AbstractValueDomain a,
        AbstractValueDomain b
    ) => ReducedProductDomain a b where
    reduction :: ReducedProduct a b -> ReducedProduct a b