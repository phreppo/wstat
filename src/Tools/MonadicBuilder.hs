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
