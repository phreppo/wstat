{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}

module Interfaces.AbstractStateDomain where

import Data.Map
import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import Interfaces.State
import Semantic.Atomic
import SyntacticStructure.WhileGrammar
import Tools.Utilities


--------------------------------------------------------------------------------
--                         Abstract State Domain
--------------------------------------------------------------------------------

class CompleteLattice d => AbstractStateDomain d where

    -- given one statement of assingment and one abstract state applies that assignment
    assign :: AtomicAssign -> d -> d

    -- given one condition statement and one abstract state applies that condition
    cond :: AtomicCond -> d -> d

--------------------------------------------------------------------------------
--                       State Domain data type
--------------------------------------------------------------------------------
--
-- For every abstarct value domain RelationalStateDomain (State Domain) should implement AbstractStateDomain interface.
-- This is beacuse in this manner one can perform pattern matching on the structure
-- of the condition or assignment.
-- One default implementation could be possible, but in Haskell we have no
-- concept of inheritance and so it would be the only one.
--

data RelationalStateDomain v b = RelationalStateDomain (Map v b)
                               | Bottom -- smashed bottom
                               deriving Eq

instance (AbstractValueDomain b, Show b) => Show (RelationalStateDomain Var b) where
    show Bottom = bottomString
    show (RelationalStateDomain domainMap) = 
        if not $ Data.Map.null domainMap 
            then "{" ++ (tail $ tail $ -- first two chars are ", "
                    foldrWithKey (\k v vs -> ", " ++ k ++ " " ++ (show v) ++ vs) "}" domainMap)
            else "{}"

-- RelationalStateDomain is a State
instance AbstractValueDomain b => State RelationalStateDomain Var b where

    lookup _   Bottom = bottom
    lookup var (RelationalStateDomain x) = x ! var

    update _   _     Bottom = Bottom 
    update var value (RelationalStateDomain x) = RelationalStateDomain $ insert var value x

    getVars Bottom = [] 
    getVars (RelationalStateDomain map) = keys map

    fromList = RelationalStateDomain . (Data.Map.fromList)

-- RelationalStateDomain is a CompleteLattice
instance AbstractValueDomain b => CompleteLattice (RelationalStateDomain Var b) where

    -- bottom :: RelationalStateDomain b
    bottom = Bottom

    -- subset :: RelationalStateDomain b -> Sb d -> Bool
    subset Bottom _      = True
    subset _      Bottom = False
    subset (RelationalStateDomain x) (RelationalStateDomain y) = all (applyPredicate subset x y) (keys x)

    -- meet :: RelationalStateDomain b -> RelationalStateDomain b -> RelationalStateDomain b
    meet Bottom _ = Bottom
    meet _ Bottom = Bottom
    meet (RelationalStateDomain x) (RelationalStateDomain y)
        | any (isBottom . applyFunction meet x y) (keys x) = Bottom -- bottom smashing
        | otherwise = mergeWithFunction meet x y

    -- join :: RelationalStateDomain b -> RelationalStateDomain b -> RelationalStateDomain b
    join = mergeStateDomainsWith join

    -- widen :: RelationalStateDomain b -> RelationalStateDomain b -> RelationalStateDomain b
    widen = mergeStateDomainsWith widen

    -- narrow :: RelationalStateDomain b -> RelationalStateDomain b -> RelationalStateDomain b
    narrow = mergeStateDomainsWith narrow

--------------------------------------------------------------------------------
-- auxiliary functions
--------------------------------------------------------------------------------

-- bottomState :: CompleteLattice b => [k] -> RelationalStateDomain k b
bottomState vars = Interfaces.State.fromList [ (var, bottom) | var <- vars]

overrideStates :: Ord v => RelationalStateDomain v b -> RelationalStateDomain v b -> RelationalStateDomain v b
overrideStates Bottom _ = Bottom
overrideStates x Bottom = x
overrideStates (RelationalStateDomain x) (RelationalStateDomain y) =
    RelationalStateDomain $ writeLeftValuesOnRightMap leftKeys x y
    where leftKeys = keys x

writeLeftValuesOnRightMap [] leftMap rightMap =
    rightMap
writeLeftValuesOnRightMap (k:ks) leftMap rightMap =
    writeLeftValuesOnRightMap ks leftMap (insert k (leftMap ! k) rightMap)

{--
function that propagate alternatives to bottom,
as described in the first two pattern match lines
take a function f and two RelationalStateDomains as params
apply f two the internal maps and then wrap the results
--}
mergeStateDomainsWith :: AbstractValueDomain b => (b -> b -> b) -> RelationalStateDomain Var b -> RelationalStateDomain Var b -> RelationalStateDomain Var b
mergeStateDomainsWith _ Bottom y = y
mergeStateDomainsWith _ x Bottom = x
mergeStateDomainsWith f (RelationalStateDomain x) (RelationalStateDomain y) = mergeWithFunction f x y

{--
take a function f and two Maps as params
apply f two the maps for each keys
note that the two maps has the same keys as precondition
--}
mergeWithFunction :: (State s k a, Ord k) =>
                     (a -> a -> a) -> Map k a -> Map k a -> s k a
mergeWithFunction f x y = Interfaces.State.fromList $
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