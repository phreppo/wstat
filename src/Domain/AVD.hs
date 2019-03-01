module Domain.AVD where

import WhileGrammar
import CompleteLattice

-- Abstract Value Domain
-- b is a powerset of abstract values
class CompleteLattice b => AVD b where

    cons :: I -> b

    rand :: SignedInfiniteInteger -> SignedInfiniteInteger -> b

    unary :: AArithemticUnaryOperator -> b -> b

    binary :: AArithemticBinOperator -> b -> b  -> b
