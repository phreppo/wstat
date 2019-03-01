{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Domain.ConcreteASD where

import Data.Map
import Domain.AVD as V
import CompleteLattice
import Domain.SD
import State
import Domain.ASD
import Semantic.Atomic
import Semantic.Evaluation
import WhileGrammar

instance AVD b => CompleteLattice (SD V b) where
    -- subset :: SD b -> Sb d -> Bool
    Bottom   `subset` _      = True
    _        `subset` Bottom = False
    (SD x)   `subset` (SD y) =
        all (\var -> (x ! var) `subset` (y ! var)) (keys x)

    -- bottom :: SD b
    bottom = Bottom

    top = error "top is useless" -- runtime error when used

    -- meet :: SD b -> SD b -> SD b
    meet Bottom _ = Bottom
    meet _ Bottom = Bottom
    meet (SD x) (SD y)
        | any (\var -> isBottom $ meet (x ! var) (y ! var)) (keys x) = Bottom
        | otherwise = State.fromList $ zip (keys x) (fmap (\var -> (x ! var) `meet` (y ! var)) (keys x))

    -- join :: SD b -> SD b -> SD b
    join Bottom y = y
    join x Bottom = x
    join (SD x) (SD y) =
        State.fromList $ zip (keys x) (fmap (\var -> (x ! var) `join` (y ! var)) (keys x))

    -- widen :: SD b -> SD b -> SD b
    widen Bottom y = y
    widen x Bottom = x
    widen (SD x) (SD y) =
        State.fromList $ zip (keys x) (fmap (\var -> (x ! var) `widen` (y ! var)) (keys x))

    -- isBottom :: d -> Bool
    -- isBottom v = v `subset` bottom

instance AVD b => ASD (SD V b) where

    -- assign :: AtomicAssign -> SD b -> SD b
    assign _ Bottom = Bottom
    assign (AtomicAssign var exp) (SD x)
        | isBottom $ eval exp (SD x) = Bottom
        | otherwise                  = SD $ insert var (eval exp (SD x)) x

    -- cond :: AtomicCond -> SD b -> SD b
    cond _ = id

instance AVD b => State SD V b where

    lookup var (SD x) = x ! var

    update var value (SD x) = SD $ insert var value x

    fromList = SD . Data.Map.fromList