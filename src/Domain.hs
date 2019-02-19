module Domain where

import WhileGrammar

-- interface for abstract domain

{--
an abstract domain is given by:
- set D# of computer-representable abstract values, implicit in the definition
  of the adt of a concrete domain, a set of mappings from variables to integer
- partial order
- monotonic concretization function,
  implicit in the definition of concrete domain?
- top and bottom in D#, Monoid require the bottom
- (optionally) galois connection
- assignments and atomic arithmetic conditions
- union and intersection
- widening operator
--}

-- TODO: should extend Monad or Monoid?
class Domain t where -- implicit t is a powerset -> t should extend Foldable?
  -- -- partial order operator
  -- subset :: t -> t -> Maybe Bool

  -- -- all-elements
  -- top :: t
  -- -- empty-set
  -- bottom :: t

  -- assignements
  assign :: AtomicAssign -> [t] -> [t]
  -- atomic arithmetic conditions, second param implicit zero
  cond :: AtomicUnaryCond -> [t] -> [t]

  -- -- abstract union
  -- join :: t -> t -> t
  -- -- abstract intersection
  -- meet :: t -> t -> t

  -- -- widening operator
  -- widen :: t -> t -> t

-- fake domain
data Tmp = T [(String, Integer)] deriving Show

instance Domain Tmp where
  assign _ = id
  cond _ = id