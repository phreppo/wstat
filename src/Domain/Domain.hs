module Domain.Domain where

import Semantic.Atomic

--------------------------------------------------------------------------------
-- interface for abstract domain
--------------------------------------------------------------------------------

type F d = Power d -> Power d

type Power d = [d]

class State t where
  ------------------------------------------------------------------------------
  -- t is State,
  -- need to access and update variables
  -- note that the instance need to deal with unbounded vars
  ------------------------------------------------------------------------------
  lookupState :: String -> t -> t
  lookupPS :: String -> Power t -> Power t
  lookupPS v []     = []
  lookupPS v (s:ss) = lookupState v s : lookupPS v ss

  updateState :: String -> Integer -> t -> t
  updatePS :: String -> Integer -> Power t -> Power t
  updatePS v x []     = []
  updatePS v x (s:ss) = updateState v x s : updatePS v x ss

class Domain t where
  ------------------------------------------------------------------------------
  -- partial order operator
  ------------------------------------------------------------------------------
  subset :: Power t -> Power t -> Maybe Bool

  ------------------------------------------------------------------------------
  -- all-elements and empty-set
  ------------------------------------------------------------------------------
  top :: Power t
  bottom :: Power t -- TODO: dove serve?

  ------------------------------------------------------------------------------
  -- Assignements and atomic arithmetic conditins
  ------------------------------------------------------------------------------
  assign :: AtomicAssign -> Power t -> Power t
  cond :: AtomicCond -> Power t -> Power t

  ------------------------------------------------------------------------------
  -- abstract union and intersection
  ------------------------------------------------------------------------------
  join :: Power t -> Power t -> Power t
  meet :: Power t -> Power t -> Power t

  -- widening operator
  widen :: Power t -> Power t -> Power t