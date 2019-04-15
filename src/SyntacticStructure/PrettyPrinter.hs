module SyntacticStructure.PrettyPrinter (prettyPrint) where

import Interfaces.AbstractStateDomain
import Interfaces.AbstractValueDomain
import Semantic.EquationSolver
import SyntacticStructure.ControlFlowGraph
import SyntacticStructure.WhileGrammar
import Tools.StateTransitions
import Tools.MonadicBuilder

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

prettyPrint :: AbstractValueDomain b => Stmt -> ProgramPointsState (NonRelationalStateDomain Var b) -> String
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
stmtPrinter stmt = cfgBuilderWithArgs stmt [
    ASSIGN (\preTab (Assign var expr) _ _ -> pure $ preTab ++ var ++ " := " ++ printAExpr expr),
    ASSERT (\preTab (Assert c) _ _ -> pure $ preTab ++ "assert " ++ printBExpr c),
    SKIP   (\preTab _ _ _ -> pure $ preTab ++ "skip"),
    SEQ    (\preTab _ s1 s2 -> addLastElement s1 ';' ++ s2),
    IF     (\preTab (If cond _ _) _ _ s1 _ _ s2 _ _ ->
        [preTab ++ "if " ++ printBExpr cond ++ " then"] ++
        s1 ++
        [preTab ++ "else "] ++
        s2 ++
        [preTab ++ "endif"]),
    WHILE  (\preTab (While cond _) _ _ _ s _ _ ->
        [preTab ++ "while " ++ printBExpr cond ++ " do"] ++ [""] ++
        s ++
        [preTab ++ "done"])
  ] (++ tab)

-- this printer build the list of program points associated to the lines of program

ppsPrinter :: Show p => Stmt -> ProgramPointsState p -> ST [String]
ppsPrinter stmt = cfgBuilderWithArgs stmt [
    ASSIGN (\pps _ _ l2 -> pure $ printProgramPoint pps l2),
    ASSERT (\pps _ _ l2 -> pure $ printProgramPoint pps l2),
    SKIP   (\_ _ _ _ -> pure ""),
    SEQ    (\_ _ -> (++)),
    IF     (\pps _ _ l2 s1 _ l4 s2 _ l6 ->
        [printProgramPoint pps l2] ++
        s1 ++
        [printProgramPoint pps l4] ++
        s2 ++
        [printProgramPoint pps l6]),
    WHILE  (\pps _ _ l2 l3 s _ l5 ->
        [printProgramPoint pps l2] ++
        [printProgramPoint pps l3] ++
        s ++
        [printProgramPoint pps l5])
  ] id

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