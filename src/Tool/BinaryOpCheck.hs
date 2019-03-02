module Tool.BinaryOpCheck where

import Interfaces.AbstractValueDomain
import Interfaces.CompleteLattice
import WhileGrammar

--------------------------------------------------------------------------------
-- auxiliary tools
--------------------------------------------------------------------------------

{--
enum, function to enumerate AVD instances Constructors
max, cardinality of AVD instances set of Constructors

this function return the length of the list of the binary function's output
of the passed AVD instances, to chekc if binary is 'totally' using a
finite set of Constructors
--}
binaryCheckOp :: (Num a, Bounded a, Enum a, AVD b) => (a -> b) -> a -> Int
binaryCheckOp enum max = length $
    do op <- [0..3]
       i1 <- [0..max]
       i2 <- [0..max]
       return $! binary (toEnum op) (enum i1) (enum i2)

-- only chekc if operations are defined for all constructors tuples
completeLatticeCheckOp enum max =
    completeLatticeCheckOp' meet enum max
    + completeLatticeCheckOp' join enum max
    + completeLatticeCheckOp' widen enum max

completeLatticeCheckOp' op enum max = length $
    do i1 <- [0..max]
       i2 <- [0..max]
       return $! op (enum i1) (enum i2)