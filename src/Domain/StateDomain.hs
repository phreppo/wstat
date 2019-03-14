{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Domain.StateDomain where

import Data.Map                       (Map, insert, fromList, (!), keys)
import Interfaces.State as S          (State(..))
import Interfaces.CompleteLattice     (bottom)
import Interfaces.AbstractValueDomain (AVD)
import SyntacticStructure.WhileGrammar                   (Var)

--------------------------------------------------------------------------------
-- State Domain data type
--------------------------------------------------------------------------------

data SD v b = SD (Map v b)
            | Bottom
            deriving (Show, Eq)

--------------------------------------------------------------------------------
-- SD is a State, whether b is AVD
--------------------------------------------------------------------------------

instance AVD b => State SD Var b where

    lookup _ Bottom = bottom
    lookup var (SD x) = x ! var

    update _ _ Bottom = Bottom -- TODO: is correct?
    update var value (SD x) = SD $ insert var value x

    fromList = SD . Data.Map.fromList

--------------------------------------------------------------------------------
-- usefull auxiliary functions
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