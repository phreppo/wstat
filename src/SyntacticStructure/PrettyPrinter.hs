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
prettyPrint tree pps = joinStmtsProgramPoints printableTree printablePPs
    where printableTree = printProgramPointsState $ sortPP $ fst $ applyST (cfgPrinter tree) startingLabel
          printablePPs = printProgramPointsState $ sortPP pps

joinStmtsProgramPoints :: [String] -> [String] -> String
joinStmtsProgramPoints [] [] = "\n"
joinStmtsProgramPoints (s:ss) (p:ps) = s ++ p ++ "\n" ++ joinStmtsProgramPoints ss ps

sortPP :: ProgramPointsState a -> [a]
sortPP [] = []
sortPP ((xLabel, xValue):xs) =
    (sortPP lesser) ++ [xValue] ++ (sortPP greater)
        where
            lesser = filter (\(label, _) -> label < xLabel) xs
            greater = filter (\(label, _) -> label >= xLabel) xs

printProgramPointsState :: Show p => [p] -> [String]
printProgramPointsState [] = []
printProgramPointsState (x:xs) = show x : printProgramPointsState xs

cfgPrinter :: Stmt -> ST (ProgramPointsState String)

cfgPrinter (Assign var expr) = do
    label1 <- fresh
    label2 <- used
    return $ [(label1, ""),(label2, var ++ " := " ++ printAExpr expr)]

cfgPrinter (Assert cond) = do
    label1 <- fresh
    label2 <- used
    return $ [(label1, "")]

-- cfgPrinter (Skip) s _ = do
--     label1 <- fresh
--     label2 <- used
--     return $ pure (label1, s Skip, label2)

-- cfgPrinter (Seq s1 s2) s c = do
--     cfgPrinter1 <- cfgPrinter s1 s c
--     cfgPrinter2 <- cfgPrinter s2 s c
--     return $ cfgPrinter1 ++ cfgPrinter2

-- cfgPrinter (If cond s1 s2) s c = do
--     label1 <- fresh
--     label2 <- used
--     cfgPrinter1 <- cfgPrinter s1 s c
--     label3 <- fresh
--     label4 <- used
--     cfgPrinter2 <- cfgPrinter s2 s c
--     label5 <- fresh
--     label6 <- used
--     return $ [
--         (label1,c cond,label2),
--         (label1,c $ BooleanUnary Not cond, label4),
--         (label3,s Skip,label6),
--         (label5,s Skip,label6)
--         ] ++ cfgPrinter1 ++ cfgPrinter2

-- cfgPrinter (While cond stmt) s c = do
--     label1 <- fresh
--     label2 <- used
--     cfgPrinter1 <- cfgPrinter stmt s c
--     label3 <- fresh
--     label4 <- used
--     return $ [
--         (label1, c cond, label2),
--         (label1, c $ BooleanUnary Not cond, label4),
--         (label3, s Skip, label1)
--         ] ++ cfgPrinter1

printAExpr :: AExpr -> String
printAExpr (IntConst i) = show i