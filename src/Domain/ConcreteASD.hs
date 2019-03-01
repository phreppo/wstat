{-# LANGUAGE FlexibleInstances #-}

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

    -- bottom :: SD b
    bottom = Bottom
    top = error "top is useless" -- runtime error when used

    -- subset :: SD b -> Sb d -> Bool
    subset Bottom _      = True
    subset _      Bottom = False
    subset (SD x) (SD y) = all (applyFunction subset x y) (keys x)

    -- meet :: SD b -> SD b -> SD b
    meet Bottom _ = Bottom
    meet _ Bottom = Bottom
    meet (SD x) (SD y)
        | any (isBottom . applyFunction meet x y) (keys x) = Bottom
        | otherwise = mergeWithFunction meet x y

    -- join :: SD b -> SD b -> SD b
    join = unit join

    -- widen :: SD b -> SD b -> SD b
    widen = unit widen

instance AVD b => ASD (SD V b) where

    -- assign :: AtomicAssign -> SD b -> SD b
    assign _ Bottom = Bottom
    assign (AtomicAssign var exp) x
        | isBottom $ eval exp x      = Bottom
        | otherwise                  = State.update var (eval exp x) x

    -- cond :: AtomicCond -> SD b -> SD b
    cond _ = id -- worst scenario
