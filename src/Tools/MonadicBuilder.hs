module Tools.MonadicBuilder where

import SyntacticStructure.WhileGrammar
import Tools.StateTransitions

type CfgFactory a = [CfgMethod a]

data CfgMethod a = ASSIGN (Stmt -> Label -> Label -> a)
                 | ASSERT (Stmt -> Label -> Label -> a)
                 | SKIP   (Stmt -> Label -> Label -> a)
                 | SEQ    (Stmt -> a -> a -> a)
                 | IF     (Stmt -> Label -> Label -> a -> Label -> Label -> a -> Label -> Label -> a)
                 | WHILE  (Stmt -> Label -> Label -> Label -> a -> Label -> Label -> a)

searchAssign :: CfgFactory a -> CfgMethod a
searchAssign ((ASSIGN x):xs) = ASSIGN x
searchAssign (_:xs) = searchAssign xs

searchAssert :: CfgFactory a -> CfgMethod a
searchAssert ((ASSERT x):xs) = ASSERT x
searchAssert (_:xs) = searchAssert xs

searchSkip :: CfgFactory a -> CfgMethod a
searchSkip ((SKIP x):xs) = SKIP x
searchSkip (_:xs) = searchSkip xs

searchSeq :: CfgFactory a -> CfgMethod a
searchSeq ((SEQ x):xs) = SEQ x
searchSeq (_:xs) = searchSeq xs

searchIf :: CfgFactory a -> CfgMethod a
searchIf ((IF x):xs) = IF x
searchIf (_:xs) = searchIf xs

searchWhile :: CfgFactory a -> CfgMethod a
searchWhile ((WHILE x):xs) = WHILE x
searchWhile (_:xs) = searchWhile xs

cfgBuilder :: Stmt -> CfgFactory a -> ST a
cfgBuilder (Assign var expr) factory = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabel
    return $ let ASSIGN f = (searchAssign factory) in f (Assign var expr) l1 l2

cfgBuilder (Assert cond) factory = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabel
    return $ let ASSERT f = (searchAssert factory) in f (Assert cond) l1 l2

cfgBuilder Skip factory = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabel
    return $ let SKIP f = (searchSkip factory) in f Skip l1 l2

cfgBuilder (Seq s1 s2) factory = do
    cfgBuilder1 <- cfgBuilder s1 factory
    cfgBuilder2 <- cfgBuilder s2 factory
    return $ let SEQ f = (searchSeq factory) in f (Seq s1 s2) cfgBuilder1 cfgBuilder2

cfgBuilder (If cond s1 s2) factory = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabel
    cfgBuilder1 <- cfgBuilder s1 factory
    l3 <- getNewLabelAndIncrement
    l4 <- getNewLabel
    cfgBuilder2 <- cfgBuilder s2 factory
    l5 <- getNewLabelAndIncrement
    l6 <- getNewLabel
    return $ let IF f = (searchIf factory) in
        f (If cond s1 s2) l1 l2 cfgBuilder1 l3 l4 cfgBuilder2 l5 l6

cfgBuilder (While cond stmt) factory = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabelAndIncrement
    l3 <- getNewLabel
    cfgBuilder1 <- cfgBuilder stmt factory
    l4 <- getNewLabelAndIncrement
    l5 <- getNewLabel
    return $ let WHILE f = (searchWhile factory) in
        f (While cond stmt) l1 l2 l3 cfgBuilder1 l4 l5


-- chooseWideningPointsMonadic ::   Stmt -> ST [Label]
-- cfg ::                           Stmt -> (Stmt -> a) -> (BExpr -> a) -> ST (ControlFlowGraph a)
-- stmtPrinter ::                   Stmt -> String -> ST [String]
-- ppsPrinter ::          Show p => Stmt -> ProgramPointsState p -> ST [String]

-- chooseWideningPointsMonadic (Assign _ _) = do
--   getNewLabelAndIncrement
--   getNewLabel
--   return []

-- chooseWideningPointsMonadic (Assert _) = do
--   getNewLabelAndIncrement
--   getNewLabel
--   return []

-- chooseWideningPointsMonadic (Skip) = do
--   getNewLabelAndIncrement
--   getNewLabel
--   return []

-- chooseWideningPointsMonadic (Seq s1 s2) = do
--   wideningPointsInS1 <- chooseWideningPointsMonadic s1
--   wideningPointsInS2 <- chooseWideningPointsMonadic s2
--   return $ wideningPointsInS1 ++ wideningPointsInS2

-- chooseWideningPointsMonadic (If _ s1 s2) = do
--   getNewLabelAndIncrement
--   getNewLabel
--   wideningPointsInThenBranch <- chooseWideningPointsMonadic s1
--   getNewLabelAndIncrement
--   getNewLabel
--   wideningPointsInElseBranch <- chooseWideningPointsMonadic s2
--   getNewLabelAndIncrement
--   getNewLabel
--   return $ wideningPointsInThenBranch ++ wideningPointsInElseBranch

-- chooseWideningPointsMonadic (While _ s) = do
--   l1 <- getNewLabelAndIncrement
--   label2 <- getNewLabelAndIncrement
--   label3 <- getNewLabel
--   wideningPointsInWhileBody <- chooseWideningPointsMonadic s
--   getNewLabelAndIncrement
--   getNewLabel
--   return $ label2:wideningPointsInWhileBody

-- cfg (Assign var expr) s _ = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure (label1, s $ Assign var expr, label2)

-- cfg (Assert cond) _ c = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure (label1, c cond, label2)

-- cfg (Skip) s _ = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure (label1, s Skip, label2)

-- cfg (Seq s1 s2) factory = do
--     cfg1 <- cfg s1 factory
--     cfg2 <- cfg s2 factory
--     return $ cfg1 ++ cfg2

-- cfg (If cond s1 s2) s c = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     cfg1 <- cfg s1 s c
--     label3 <- getNewLabelAndIncrement
--     label4 <- getNewLabel
--     cfg2 <- cfg s2 s c
--     label5 <- getNewLabelAndIncrement
--     label6 <- getNewLabel
--     return $ [
--         (label1,c cond,label2),
--         (label1,c $ BooleanUnary Not cond, label4),
--         (label3,s Skip,label6),
--         (label5,s Skip,label6)
--         ] ++ cfg1 ++ cfg2

-- cfg (While cond stmt) s c = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabelAndIncrement
--     label3 <- getNewLabel
--     cfg1 <- cfg stmt s c
--     label4 <- getNewLabelAndIncrement
--     label5 <- getNewLabel
--     return $ [
--         (label1, s Skip, label2),
--         (label2, c cond, label3),
--         (label2, c $ BooleanUnary Not cond, label5),
--         (label4, s Skip, label2)
--         ] ++ cfg1

-- stmtPrinter :: Stmt -> String -> ST [String]

-- stmtPrinter (Assign var expr) preTab = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure $ preTab ++ var ++ " := " ++ printAExpr expr

-- stmtPrinter (Assert c) preTab = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure $ preTab ++ "assert " ++ printBExpr c

-- stmtPrinter (Skip) preTab = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure $ preTab ++ "skip"

-- stmtPrinter (Seq s1 s2) preTab = do
--     stmtPrinter1 <- stmtPrinter s1 preTab
--     stmtPrinter2 <- stmtPrinter s2 preTab
--     return $ addLastElement stmtPrinter1 ';' ++ stmtPrinter2

-- stmtPrinter (If cond s1 s2) preTab = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     stmtPrinter1 <- stmtPrinter s1 (preTab ++ tab)
--     label3 <- getNewLabelAndIncrement
--     label4 <- getNewLabel
--     stmtPrinter2 <- stmtPrinter s2 (preTab ++ tab)
--     label5 <- getNewLabelAndIncrement
--     label6 <- getNewLabel
--     return $ [preTab ++ "if " ++ printBExpr cond ++ " then"] ++
--              stmtPrinter1 ++
--              [preTab ++ "else "] ++
--              stmtPrinter2 ++
--              [preTab ++ "endif"]

-- stmtPrinter (While cond stmt) preTab = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabelAndIncrement
--     label3 <- getNewLabel
--     stmtPrinter1 <- stmtPrinter stmt (preTab ++ tab)
--     label4 <- getNewLabelAndIncrement
--     label5 <- getNewLabel
--     return $ [preTab ++ "while " ++ printBExpr cond ++ " do"] ++ [""] ++
--              stmtPrinter1 ++
--              [preTab ++ "done"]

-- ppsPrinter (Assign var expr) pps  = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure $ printProgramPoint pps label2

-- ppsPrinter (Assert c) pps  = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure $ printProgramPoint pps label2

-- ppsPrinter (Skip) pps  = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     return $ pure ""

-- ppsPrinter (Seq s1 s2) pps  = do
--     ppsPrinter1 <- ppsPrinter s1 pps
--     ppsPrinter2 <- ppsPrinter s2 pps
--     return $ ppsPrinter1 ++ ppsPrinter2

-- ppsPrinter (If cond s1 s2) pps  = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabel
--     ppsPrinter1 <- ppsPrinter s1 pps
--     label3 <- getNewLabelAndIncrement
--     label4 <- getNewLabel
--     ppsPrinter2 <- ppsPrinter s2 pps
--     label5 <- getNewLabelAndIncrement
--     label6 <- getNewLabel
--     return $ [printProgramPoint pps label2] ++
--              ppsPrinter1 ++
--              [printProgramPoint pps label4] ++
--              ppsPrinter2 ++
--              [printProgramPoint pps label6]

-- ppsPrinter (While cond stmt) pps  = do
--     label1 <- getNewLabelAndIncrement
--     label2 <- getNewLabelAndIncrement
--     label3 <- getNewLabel
--     ppsPrinter1 <- ppsPrinter stmt pps
--     label4 <- getNewLabelAndIncrement
--     label5 <- getNewLabel
--     return $ [printProgramPoint pps label2] ++
--              [printProgramPoint pps label3] ++
--              ppsPrinter1 ++
--              [printProgramPoint pps label5]