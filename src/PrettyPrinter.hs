module PrettyPrinter where

import WhileGrammar
import Domain.Domain
import Equation.EquationList

pprint :: Stmt -> String
pprint Skip = "skip;\n"
