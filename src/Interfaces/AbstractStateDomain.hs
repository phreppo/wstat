{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}

module Interfaces.AbstractStateDomain where

import Data.Map
import Interfaces.AbstractValueDomain
import Interfaces.AbstractDomain
import Interfaces.State
import Semantic.Atomic
import SyntacticStructure.WhileGrammar
import Tools.Utilities


--------------------------------------------------------------------------------
--                         Abstract State Domain
--------------------------------------------------------------------------------

class AbstractDomain d => AbstractStateDomain d where

    -- given one statement of assingment and one abstract state applies that assignment
    assign :: AtomicAssign -> d -> d

    -- given one condition statement and one abstract state applies that condition
    cond :: AtomicCond -> d -> d

--------------------------------------------------------------------------------
--                       State Domain data type
--------------------------------------------------------------------------------
--
-- For every abstarct value domain NonRelationalStateDomain (State Domain) should implement AbstractStateDomain interface.
-- This is beacuse in this manner one can perform pattern matching on the structure
-- of the condition or assignment.
-- One default implementation could be possible, but in Haskell we have no
-- concept of inheritance and so it would be the only one.
--

data NonRelationalStateDomain v b = NonRelationalStateDomain (Map v b)
                               | Bottom -- smashed bottom
                               deriving Eq

instance (AbstractValueDomain b, Show b) => Show (NonRelationalStateDomain Var b) where
    show Bottom = bottomString
    show (NonRelationalStateDomain domainMap) =
        if not $ Data.Map.null domainMap
            then "{" ++ (tail $ tail $ -- first two chars are ", "
                    foldrWithKey (\k v vs -> ", " ++ k ++ " " ++ (show v) ++ vs) "}" domainMap)
            else "{}"

-- NonRelationalStateDomain is a State
instance AbstractValueDomain b => State NonRelationalStateDomain Var b where

    lookup _   Bottom = bottom
    lookup var (NonRelationalStateDomain x) = x ! var

    update _   _     Bottom = Bottom
    update var value (NonRelationalStateDomain x) = NonRelationalStateDomain $ insert var value x

    getVars Bottom = []
    getVars (NonRelationalStateDomain map) = keys map

    fromList = NonRelationalStateDomain . (Data.Map.fromList)

-- NonRelationalStateDomain is a AbstractDomain
-- since it is derived by Smashed Point-wise Lifting from the given AbstractValueDomain
instance AbstractValueDomain b => AbstractDomain (NonRelationalStateDomain Var b) where

    -- bottom :: NonRelationalStateDomain b
    bottom = Bottom

    -- top :: NonRelationalStateDomain b
    top = error "[NonRelationalStateDomain] Top is not needed"


    -- subset :: NonRelationalStateDomain b -> Sb d -> Bool
    subset Bottom _      = True
    subset _      Bottom = False
    subset (NonRelationalStateDomain x) (NonRelationalStateDomain y) = all (applyPredicate subset x y) (keys x)

    -- meet :: NonRelationalStateDomain b -> NonRelationalStateDomain b -> NonRelationalStateDomain b
    meet Bottom _ = Bottom
    meet _ Bottom = Bottom
    meet (NonRelationalStateDomain x) (NonRelationalStateDomain y)
        | any (isBottom . applyFunction meet x y) (keys x) = Bottom -- bottom smashing
        | otherwise = mergeWithFunction meet x y

    -- join :: NonRelationalStateDomain b -> NonRelationalStateDomain b -> NonRelationalStateDomain b
    join = mergeStateDomainsWith join

    -- widen :: NonRelationalStateDomain b -> NonRelationalStateDomain b -> NonRelationalStateDomain b
    widen = mergeStateDomainsWith widen

    -- narrow :: NonRelationalStateDomain b -> NonRelationalStateDomain b -> NonRelationalStateDomain b
    narrow = mergeStateDomainsWith narrow

--------------------------------------------------------------------------------
-- auxiliary functions
--------------------------------------------------------------------------------

-- bottomState :: AbstractDomain b => [k] -> NonRelationalStateDomain k b
bottomState vars = Interfaces.State.fromList [ (var, bottom) | var <- vars]

overrideStates :: Ord v => NonRelationalStateDomain v b -> NonRelationalStateDomain v b -> NonRelationalStateDomain v b
overrideStates Bottom _ = Bottom
overrideStates x Bottom = x
overrideStates (NonRelationalStateDomain x) (NonRelationalStateDomain y) =
    NonRelationalStateDomain $ writeLeftValuesOnRightMap leftKeys x y
    where leftKeys = keys x

writeLeftValuesOnRightMap [] leftMap rightMap =
    rightMap
writeLeftValuesOnRightMap (k:ks) leftMap rightMap =
    writeLeftValuesOnRightMap ks leftMap (insert k (leftMap ! k) rightMap)

{--
function that propagate alternatives to bottom,
as described in the first two pattern match lines
take a function f and two NonRelationalStateDomains as params
apply f two the internal maps and then wrap the results
--}
mergeStateDomainsWith :: AbstractValueDomain b => (b -> b -> b) -> NonRelationalStateDomain Var b -> NonRelationalStateDomain Var b -> NonRelationalStateDomain Var b
mergeStateDomainsWith _ Bottom y = y
mergeStateDomainsWith _ x Bottom = x
mergeStateDomainsWith f (NonRelationalStateDomain x) (NonRelationalStateDomain y) = mergeWithFunction f x y

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