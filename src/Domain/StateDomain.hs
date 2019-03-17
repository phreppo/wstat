{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Domain.StateDomain where

import Data.Map                       (Map, insert, fromList, (!), keys)
import Interfaces.State as S          (State(..))
import Interfaces.CompleteLattice     (CompleteLattice(..))
import Data.Map                       (keys)
import Interfaces.AbstractValueDomain (AVD)
import Interfaces.CompleteLattice     (CompleteLattice(..))
import Interfaces.State               (update)
import Interfaces.AbstractStateDomain (ASD(..))
import Semantic.Atomic                (AtomicAssign(..))
import Semantic.Evaluation            (abstractEval)
import SyntacticStructure.WhileGrammar                   (Var)
import Interfaces.AbstractValueDomain (AVD)
import SyntacticStructure.WhileGrammar                   (Var)

--------------------------------------------------------------------------------
--                      State Domain data type
--------------------------------------------------------------------------------
-- 
-- For every abstarct value domain SD (State Domain) should implement ASD interface.
-- This is beacuse in this manner one can perform pattern matching on the structure
-- of the condition or assignment.
-- One default implementation could be possible, but in Haskell we have no 
-- concept of inheritance and so it would be the only one.
-- 

data SD v b = SD (Map v b)
            | Bottom -- smashed bottom
            deriving (Show, Eq)

-- SD is a State
instance AVD b => State SD Var b where

    lookup _   Bottom = bottom
    lookup var (SD x) = x ! var

    update _   _     Bottom = Bottom -- TODO: is this correct?
    update var value (SD x) = SD $ insert var value x

    fromList = SD . (Data.Map.fromList)

-- SD is a CompleteLattice
instance AVD b => CompleteLattice (SD Var b) where

    -- bottom :: SD b
    bottom = Bottom

    -- subset :: SD b -> Sb d -> Bool
    subset Bottom _      = True
    subset _      Bottom = False
    subset (SD x) (SD y) = all (applyPredicate subset x y) (keys x)

    -- meet :: SD b -> SD b -> SD b
    meet Bottom _ = Bottom
    meet _ Bottom = Bottom
    meet (SD x) (SD y)
        | any (isBottom . applyFunction meet x y) (keys x) = Bottom -- bottom smashing
        | otherwise = mergeWithFunction meet x y

    -- join :: SD b -> SD b -> SD b
    join = mergeStateDomainsWith join

    -- widen :: SD b -> SD b -> SD b
    widen = mergeStateDomainsWith widen

--------------------------------------------------------------------------------
-- useful auxiliary functions
--------------------------------------------------------------------------------

{--
function that propagate alternatives to bottom,
as described in the first two pattern match lines
take a function f and two SDs as params
apply f two the internal maps and then wrap the results
--}
mergeStateDomainsWith :: AVD b => (b -> b -> b) -> SD Var b -> SD Var b -> SD Var b
mergeStateDomainsWith _ Bottom y = y
mergeStateDomainsWith _ x Bottom = x
mergeStateDomainsWith f (SD x) (SD y) = mergeWithFunction f x y

{--
take a function f and two Maps as params
apply f two the maps for each keys
note that the two maps has the same keys as precondition
--}
mergeWithFunction :: (State s k a, Ord k) =>
                     (a -> a -> a) -> Map k a -> Map k a -> s k a
mergeWithFunction f x y = S.fromList $
    zip (keys x) (fmap (applyFunction f x y) (keys x))

{--
take a function f and two Maps as params and then a single key
apply f two the relative value extracted using the passed key
note that the two maps has the same keys as precondition
--}
applyFunction :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> k -> c
applyFunction f x y = \var -> f (x ! var) (y ! var)

applyPredicate :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> k -> Bool
applyPredicate = applyFunction