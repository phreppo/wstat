module SyntacticStructure.PrettyPrinter (prettyPrint) where

import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Semantic.EquationSolver
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.WhileGrammar
import Tools.StateTransitions

--------------------------------------------------------------------------------
--                         Pretty Printer Costant
--------------------------------------------------------------------------------

-- now we define format constant to print in a pretty way the program with
-- program points associated

-- this const define the number of char (in absolute value) where
-- the program points being to print
ppsLineStarter :: Int
ppsLineStarter = 20

-- tabLength is the length of a tab char, tab function is the string representing
-- this custom-length tab
tabLength :: Int
tabLength = 4

tab :: String
tab = [' ' | _ <- [0..tabLength]]

-- this is the program points separator, should be the comment token of while
ppsSeparator :: String
ppsSeparator = " # "

--------------------------------------------------------------------------------
--                               Pretty Printer
--------------------------------------------------------------------------------

prettyPrint :: AbstractValueDomain b => Stmt -> ProgramPointsState (RelationalStateDomain Var b) -> String
prettyPrint tree pps = joinStmtsProgramPoints printableTree printablePPs ppsLineStarter
    where printableTree = "": (fst $ applyST (stmtPrinter tree "") startingLabel)
          printablePPs = (printProgramPoint pps startingLabel) :
                         (fst $ applyST (ppsPrinter tree pps) startingLabel)

-- join the lines of code and the program points at the right indentation
joinStmtsProgramPoints :: [String] -> [String] -> Int -> String
joinStmtsProgramPoints [] [] _ = "\n"
joinStmtsProgramPoints (s:ss) (p:ps) ppsLine = s ++ spaces ++ p ++ "\n" ++ joinStmtsProgramPoints ss ps ppsLine
    where spaces = [' ' | _ <- [0..ppsLine - (length s + tabsIn s)]]
          tabsIn string = tabLength * length (filter (== '\t') string)

-- add a character (always the ';' char) only at the end of the last line of the list passed
addLastElement :: [String] -> Char -> [String]
addLastElement [] _ = []
addLastElement [s] c = pure $ s ++ pure c
addLastElement (s:ss) c = s : addLastElement ss c

printProgramPoint :: Show p => ProgramPointsState p -> Label -> String
printProgramPoint pps l = ppsSeparator ++ "[" ++ show l ++ "] " ++ searchString pps l ++ ppsSeparator

-- searchString for a label inside the program points data type
searchString :: Show p => ProgramPointsState p -> Label -> String
searchString pps label = show $ retrieveProgramPointState pps label

--------------------------------------------------------------------------------
--                          Monadic Printers
--------------------------------------------------------------------------------

-- the first printer build the list of lines of the program

stmtPrinter :: Stmt -> String -> ST [String]

stmtPrinter (Assign var expr) preTab = do
    label1 <- fresh
    label2 <- used
    return $ pure $ preTab ++ var ++ " := " ++ printAExpr expr

stmtPrinter (Assert c) preTab = do
    label1 <- fresh
    label2 <- used
    return $ pure $ preTab ++ "assert " ++ printBExpr c

stmtPrinter (Skip) preTab = do
    label1 <- fresh
    label2 <- used
    return $ pure $ preTab ++ "skip"

stmtPrinter (Seq s1 s2) preTab = do
    stmtPrinter1 <- stmtPrinter s1 preTab
    stmtPrinter2 <- stmtPrinter s2 preTab
    return $ addLastElement stmtPrinter1 ';' ++ stmtPrinter2

stmtPrinter (If cond s1 s2) preTab = do
    label1 <- fresh
    label2 <- used
    stmtPrinter1 <- stmtPrinter s1 (preTab ++ tab)
    label3 <- fresh
    label4 <- used
    stmtPrinter2 <- stmtPrinter s2 (preTab ++ tab)
    label5 <- fresh
    label6 <- used
    return $ [preTab ++ "if " ++ printBExpr cond ++ " then"] ++
             stmtPrinter1 ++
             [preTab ++ "else "] ++
             stmtPrinter2 ++
             [preTab ++ "endif"]

stmtPrinter (While cond stmt) preTab = do
    label1 <- fresh
    label2 <- fresh
    label3 <- used
    stmtPrinter1 <- stmtPrinter stmt (preTab ++ tab)
    label4 <- fresh
    label5 <- used
    return $ [preTab ++ "while " ++ printBExpr cond ++ " do"] ++ [""] ++
             stmtPrinter1 ++
             [preTab ++ "done"]

-- this printer build the list of program points associated to the lines of program

ppsPrinter :: Show p => Stmt -> ProgramPointsState p -> ST [String]

ppsPrinter (Assign var expr) pps  = do
    label1 <- fresh
    label2 <- used
    return $ pure $ printProgramPoint pps label2

ppsPrinter (Assert c) pps  = do
    label1 <- fresh
    label2 <- used
    return $ pure $ printProgramPoint pps label2

ppsPrinter (Skip) pps  = do
    label1 <- fresh
    label2 <- used
    return $ pure ""

ppsPrinter (Seq s1 s2) pps  = do
    ppsPrinter1 <- ppsPrinter s1 pps
    ppsPrinter2 <- ppsPrinter s2 pps
    return $ ppsPrinter1 ++ ppsPrinter2

ppsPrinter (If cond s1 s2) pps  = do
    label1 <- fresh
    label2 <- used
    ppsPrinter1 <- ppsPrinter s1 pps
    label3 <- fresh
    label4 <- used
    ppsPrinter2 <- ppsPrinter s2 pps
    label5 <- fresh
    label6 <- used
    return $ [printProgramPoint pps label2] ++
             ppsPrinter1 ++
             [printProgramPoint pps label4] ++
             ppsPrinter2 ++
             [printProgramPoint pps label6]

ppsPrinter (While cond stmt) pps  = do
    label1 <- fresh
    label2 <- fresh
    label3 <- used
    ppsPrinter1 <- ppsPrinter stmt pps
    label4 <- fresh
    label5 <- used
    return $ [printProgramPoint pps label2] ++
             [printProgramPoint pps label3] ++
             ppsPrinter1 ++
             [printProgramPoint pps label5]

--------------------------------------------------------------------------------
--                        AExpr and BExpr printer
--------------------------------------------------------------------------------

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
printBExpr (ArithmeticBinary IsEqual c1 c2) = "(" ++ printAExpr c1 ++ " = " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary IsNEqual c1 c2) = "(" ++ printAExpr c1 ++ " != " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary Less c1 c2) = "(" ++ printAExpr c1 ++ " < " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary Greater c1 c2) = "(" ++ printAExpr c1 ++ " > " ++ printAExpr c2 ++ ")"
printBExpr (ArithmeticBinary GreaterEq c1 c2) = "(" ++ printAExpr c1 ++ " >= " ++ printAExpr c2 ++ ")"