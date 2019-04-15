module Tools.MonadicBuilder (CfgFactoryWithArgs, CfgMethodWithArgs(..), cfgBuilderWithArgs) where

import SyntacticStructure.WhileGrammar
import Tools.StateTransitions

type CfgFactoryWithArgs a b = [CfgMethodWithArgs a b]

data CfgMethodWithArgs a b =
    ASSIGN (b -> Stmt -> Label -> Label -> a) |
    ASSERT (b -> Stmt -> Label -> Label -> a) |
    SKIP   (b -> Stmt -> Label -> Label -> a) |
    SEQ    (b -> Stmt -> a -> a -> a) |
    IF     (b -> Stmt -> Label -> Label -> a -> Label -> Label -> a -> Label -> Label -> a) |
    WHILE  (b -> Stmt -> Label -> Label -> Label -> a -> Label -> Label -> a)

searchAssign :: CfgFactoryWithArgs a b -> CfgMethodWithArgs a b
searchAssign ((ASSIGN x):xs) = ASSIGN x
searchAssign (_:xs) = searchAssign xs

searchAssert :: CfgFactoryWithArgs a b -> CfgMethodWithArgs a b
searchAssert ((ASSERT x):xs) = ASSERT x
searchAssert (_:xs) = searchAssert xs

searchSkip :: CfgFactoryWithArgs a b -> CfgMethodWithArgs a b
searchSkip ((SKIP x):xs) = SKIP x
searchSkip (_:xs) = searchSkip xs

searchSeq :: CfgFactoryWithArgs a b -> CfgMethodWithArgs a b
searchSeq ((SEQ x):xs) = SEQ x
searchSeq (_:xs) = searchSeq xs

searchIf :: CfgFactoryWithArgs a b -> CfgMethodWithArgs a b
searchIf ((IF x):xs) = IF x
searchIf (_:xs) = searchIf xs

searchWhile :: CfgFactoryWithArgs a b -> CfgMethodWithArgs a b
searchWhile ((WHILE x):xs) = WHILE x
searchWhile (_:xs) = searchWhile xs

cfgBuilderWithArgs :: Stmt -> CfgFactoryWithArgs a b -> (b -> b) -> b -> ST a
cfgBuilderWithArgs (Assign var expr) factory _ args = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabel
    return $ let ASSIGN f = (searchAssign factory) in f args (Assign var expr) l1 l2

cfgBuilderWithArgs (Assert cond) factory _ args = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabel
    return $ let ASSERT f = (searchAssert factory) in f args (Assert cond) l1 l2

cfgBuilderWithArgs Skip factory _ args = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabel
    return $ let SKIP f = (searchSkip factory) in f args Skip l1 l2

cfgBuilderWithArgs (Seq s1 s2) factory f args = do
    cfgBuilderWithArgs1 <- cfgBuilderWithArgs s1 factory f args
    cfgBuilderWithArgs2 <- cfgBuilderWithArgs s2 factory f args
    return $ let SEQ f = (searchSeq factory) in f args (Seq s1 s2) cfgBuilderWithArgs1 cfgBuilderWithArgs2

cfgBuilderWithArgs (If cond s1 s2) factory computeArgs args = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabel
    cfgBuilderWithArgs1 <- cfgBuilderWithArgs s1 factory computeArgs (computeArgs args)
    l3 <- getNewLabelAndIncrement
    l4 <- getNewLabel
    cfgBuilderWithArgs2 <- cfgBuilderWithArgs s2 factory computeArgs (computeArgs args)
    l5 <- getNewLabelAndIncrement
    l6 <- getNewLabel
    return $ let IF f = (searchIf factory) in
        f args (If cond s1 s2) l1 l2 cfgBuilderWithArgs1 l3 l4 cfgBuilderWithArgs2 l5 l6

cfgBuilderWithArgs (While cond stmt) factory computeArgs args = do
    l1 <- getNewLabelAndIncrement
    l2 <- getNewLabelAndIncrement
    l3 <- getNewLabel
    cfgBuilderWithArgs1 <- cfgBuilderWithArgs stmt factory computeArgs (computeArgs args)
    l4 <- getNewLabelAndIncrement
    l5 <- getNewLabel
    return $ let WHILE f = (searchWhile factory) in
        f args (While cond stmt) l1 l2 l3 cfgBuilderWithArgs1 l4 l5