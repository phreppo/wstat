{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Interfaces.ReducedProductDomain where

--
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Interfaces.AbstractDomain
import SyntacticStructure.WhileGrammar

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


instance (
    AbstractStateDomain a,
    AbstractStateDomain b,
    ReducedProductDomain a b
  ) => AbstractStateDomain (ReducedProduct a b) where

    assign exp (x, y) = reduction (assign exp x, assign exp y)

    cond c (x, y) = reduction (cond c x, cond c y)

    -- assign exp state = fromList $ zip (getVars state) (fmap (\k -> let (x, y) = Interfaces.State.lookup k state in reduction (assign exp x, assign exp y)) (getVars state))

    -- cond c state = fromList $ zip (getVars state) (fmap (\k -> let (x, y) = Interfaces.State.lookup k state in reduction (cond c x, cond c y)) (getVars state))


class (
        AbstractValueDomain a,
        AbstractValueDomain b
    ) => ReducedProductDomain a b where
    reduction :: ReducedProduct a b -> ReducedProduct a b