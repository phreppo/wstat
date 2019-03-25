{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module SyntacticStructure.PrettyPrinter (prettyPrint) where

import Tools.StateTransitions
import Semantic.EquationSolver
import SyntacticStructure.WhileGrammar
import SyntacticStructure.ControlFlowGraph
import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain

--------------------------------------------------------------------------------
--                             Pretty Printer
--------------------------------------------------------------------------------

prettyPrint :: AVD b => Stmt -> ProgramPointsState (SD Var b) -> String
prettyPrint tree pps = "\n" ++ (fst $ applyST (cfgPrinter tree pps "") startingLabel) ++ "\n"

-- joinStmtsProgramPoints :: [String] -> [String] -> String
-- joinStmtsProgramPoints [] [] = "\n"
-- joinStmtsProgramPoints (s:ss) (p:ps) = s ++ p ++ "\n" ++ joinStmtsProgramPoints ss ps

-- sortPP :: ProgramPointsState a -> [a]
-- sortPP [] = []
-- sortPP ((xLabel, xValue):xs) =
--     (sortPP lesser) ++ [xValue] ++ (sortPP greater)
--         where
--             lesser = filter (\(label, _) -> label < xLabel) xs
--             greater = filter (\(label, _) -> label >= xLabel) xs

-- printProgramPointsState :: Show p => [p] -> [String]
-- printProgramPointsState [] = []
-- printProgramPointsState (x:xs) = show x : printProgramPointsState xs

search :: Show p => ProgramPointsState p -> Label -> String
search ((l, v):pps) label | l == label = show v
                          | otherwise = search pps label

cfgPrinter :: Show p => Stmt -> ProgramPointsState p -> String -> ST String


cfgPrinter (Assign var expr) pps preTab = do
    label1 <- fresh
    label2 <- used
    return $ preTab ++ var ++ " := " ++ printAExpr expr ++ "; // " ++ search pps label2

cfgPrinter (Assert c) pps preTab = do
    label1 <- fresh
    label2 <- used
    return $ preTab ++ "assert " ++ printBExpr c ++ "; // " ++ search pps label2

cfgPrinter (Skip) pps preTab = do
    label1 <- fresh
    label2 <- used
    return $ preTab ++ "skip;"

cfgPrinter (Seq s1 s2) pps preTab = do
    cfgPrinter1 <- cfgPrinter s1 pps preTab
    cfgPrinter2 <- cfgPrinter s2 pps preTab
    return $ cfgPrinter1 ++ "\n" ++ cfgPrinter2

cfgPrinter (If cond s1 s2) pps preTab = do
    label1 <- fresh
    label2 <- used
    cfgPrinter1 <- cfgPrinter s1 pps (preTab ++ "\t")
    label3 <- fresh
    label4 <- used
    cfgPrinter2 <- cfgPrinter s2 pps (preTab ++ "\t")
    label5 <- fresh
    label6 <- used
    return $ preTab ++ "if " ++ printBExpr cond ++ " then // " ++ search pps label2 ++
             "\n" ++ cfgPrinter1 ++ "\n" ++
             preTab ++ "else // " ++ search pps label4 ++
             "\n" ++ cfgPrinter2 ++ "\n" ++ preTab ++ "endif; // " ++ search pps label6

cfgPrinter (While cond stmt) pps preTab = do
    label1 <- fresh
    label2 <- used
    cfgPrinter1 <- cfgPrinter stmt pps (preTab ++ "\t")
    label3 <- fresh
    label4 <- used
    return $ preTab ++ "while " ++ printBExpr cond ++ " do // " ++ search pps label2 ++
             "\n" ++ cfgPrinter1 ++ "\n" ++
             preTab ++ "done; // " ++ search pps label4

printAExpr :: AExpr -> String
printAExpr (IntConst i) = show i
printAExpr (NonDet i1 i2) = "[" ++ show i1 ++ ", " ++ show i2 ++ "]"
printAExpr (Var v) = v
printAExpr (AUnary Neg exp) = "(- " ++ printAExpr exp ++ ")"
printAExpr (ABinary Add e1 e2) = "(" ++ printAExpr e1 ++ " + " ++ printAExpr e2 ++ ")"
printAExpr (ABinary Subtract e1 e2) = "(" ++ printAExpr e1 ++ " - " ++ printAExpr e2 ++ ")"
printAExpr (ABinary Multiply e1 e2) = "(" ++ printAExpr e1 ++ " * " ++ printAExpr e2 ++ ")"
printAExpr (ABinary Division e1 e2) = "(" ++ printAExpr e1 ++ " / " ++ printAExpr e2 ++ ")"

printBExpr :: BExpr -> String
printBExpr (BoolConst c) = show c
printBExpr (BooleanUnary Not c) = "(not " ++ printBExpr c ++ ")"
printBExpr (BooleanBinary And c1 c2) = "(" ++ printBExpr c1 ++ " and " ++ printBExpr c2 ++ ")"
printBExpr (BooleanBinary Or c1 c2) = "(" ++ printBExpr c1 ++ " or " ++ printBExpr c2 ++ ")"
printBExpr (ArithmeticBinary LessEq c1 c2) = "(" ++ printAExpr c1 ++ " <= " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary IsEqual c1 c2) = "(" ++ printAExpr c1 ++ " == " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary IsNEqual c1 c2) = "(" ++ printAExpr c1 ++ " != " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary Less c1 c2) = "(" ++ printAExpr c1 ++ " < " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary Greater c1 c2) = "(" ++ printAExpr c1 ++ " > " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary GreaterEq c1 c2) = "(" ++ printAExpr c1 ++ " >= " ++ printAExpr c2 ++ ")"