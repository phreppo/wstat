module Domain.Domain where

import Semantic.Atomic

--------------------------------------------------------------------------------
-- interface for abstract domain
--------------------------------------------------------------------------------

class Domain t where
  ------------------------------------------------------------------------------
  -- t is State,
  -- need to access and update variables
  -- note that the instance need to deal with unbounded vars
  ------------------------------------------------------------------------------
  lookupState :: String -> t -> t
  lookupPS :: String -> [t] -> [t]
  lookupPS v []     = []
  lookupPS v (s:ss) = lookupState v s : lookupPS v ss

  updateState :: String -> Integer -> t -> t
  updatePS :: String -> Integer -> [t] -> [t]
  updatePS v x []     = []
  updatePS v x (s:ss) = updateState v x s : updatePS v x ss

  ------------------------------------------------------------------------------
  -- partial order operator
  ------------------------------------------------------------------------------
  subset :: [t] -> [t] -> Maybe Bool

  ------------------------------------------------------------------------------
  -- all-elements and empty-set
  ------------------------------------------------------------------------------
  top :: [t]
  bottom :: [t]

  ------------------------------------------------------------------------------
  -- Assignements and atomic arithmetic conditins
  ------------------------------------------------------------------------------
  assign :: AtomicAssign -> [t] -> [t]
  cond :: AtomicCond -> [t] -> [t]

  ------------------------------------------------------------------------------
  -- abstract union and intersection
  ------------------------------------------------------------------------------
  join :: [t] -> [t] -> [t]
  meet :: [t] -> [t] -> [t]

  -- widening operator
  widen :: [t] -> [t] -> [t]