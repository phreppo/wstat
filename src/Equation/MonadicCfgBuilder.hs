module Equation.MonadicCfgBuilder where

-- this is the monadic cfg builder
-- using the fact that EQM is Monad doesn't improve the code

-- import Equation.EquationList
-- import Data.Monoid
-- import Domain.Domain
-- import WhileGrammar
-- import Semantic.Atomic
-- import Semantic.Condition

-- unit :: -- Domain d =>
--         Char -> EQM ([Equation (Char)])
-- unit x = EQM (\l -> ([Equation (l, x, nextLabel l)], nextLabel l))

-- comp :: Monoid a => a -> a -> EQM a
-- xs `comp` ys = EQM (\l -> (xs `mappend` ys, l))

-- (<|>) :: EQM [Equation (Char)] -> EQM [Equation (Char)] -> BExpr -> EQM [Equation (Char)]
-- (<|>) eqx eqy c = EQM (\l1 -> let (x, l') = applyEQM eqx l
--                                  (y, l'') = applyEQM eqy (nextLabel l')
--                                  init = [Equation ()]
--                                  eof = [Equation (l', '0', nextLabel l''),
--                                         Equation (l'', '0', nextLabel l'')] in
--                                  (init ++ x ++ y ++ eof, nextLabel l''))

-- cfg :: -- Domain d =>
--         Stmt -> EQM ([Equation (Char)])
-- -- base cases
-- cfg (Assign var expr) = unit $ 'a' -- assign $ AtomicAssign var expr
-- cfg (Assert c) = unit $ 'b' -- condition $ c
-- cfg (Skip) = unit 'c' -- id

-- -- recursive cases
-- cfg (Seq s1 s2) = do xs1 <- cfg s1 -- xs1 :: [Equation (F d)]
--                      xs2 <- cfg s2 -- xs2 :: [Equation (F d)]
--                      xs1 `comp` xs2

-- cfg (If c s1 s2) = cfg s1 <|> cfg s2 $ c
