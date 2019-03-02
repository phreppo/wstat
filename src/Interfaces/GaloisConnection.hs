module Interfaces.GaloisConnection where

import Interfaces.CompleteLattice
import WhileGrammar

class CompleteLattice b => GaloisConnection b where

    abstraction :: I -> b