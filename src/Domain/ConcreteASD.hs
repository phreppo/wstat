module Domain.ConcreteASD where

import Domain.AVD as V
    
import Domain.ASD 
import Data.Map
import Semantic.Atomic
    
data SD b = SD (Map String b)
          | Bottom
          deriving (Show)

instance AVD b => ASD (SD b) where
    -- subset :: SD b -> Sb d -> Bool
    Bottom   `subset` _      = True
    _        `subset` Bottom = False
    (SD x)   `subset` (SD y) =
        all (\var -> (x ! var) `V.subset` (y ! var)) (keys x)

    -- bottom :: SD b
    bottom = Bottom

    -- assign :: AtomicAssign -> SD b -> SD b
    -- TODO: more precise way to do this, URGENT
    assign _ Bottom = Bottom
    assign (AtomicAssign var exp) (SD x) =
        SD $ insert var top x  

    -- cond :: AtomicCond -> SD b -> SD b
    cond _ = id

    -- meet :: SD b -> SD b -> SD b
    meet Bottom _ = Bottom
    meet _ Bottom = Bottom
    meet (SD x) (SD y) 
        | any (\var -> V.isBottom $ V.meet (x ! var) (y ! var)) (keys x) = Bottom 
        | otherwise = SD $ fromList $ zip (keys x) (fmap (\var -> (x ! var) `V.meet` (y ! var)) (keys x))

    -- join :: SD b -> SD b -> SD b
    join Bottom y = y
    join x Bottom = x
    join (SD x) (SD y) =
        SD $ fromList $ zip (keys x) (fmap (\var -> (x ! var) `V.join` (y ! var)) (keys x)) 

    -- widen :: SD b -> SD b -> SD b
    widen Bottom y = y
    widen x Bottom = x
    widen (SD x) (SD y) =
        SD $ fromList $ zip (keys x) (fmap (\var -> (x ! var) `V.widen` (y ! var)) (keys x)) 

    -- isBottom :: d -> Bool
    -- isBottom v = v `subset` bottom
    