module Equation.EquationList where

--------------------------------------------------------------------------------
-- Equation Abstract Data Type
--------------------------------------------------------------------------------

type Equation l a = (l, a, l)

-- TODO: make this a set
-- X list
type EqList l a = ([Equation l a], l)

getCtx :: Equation Label a -> a
getCtx (_, x, _) = x

equals :: Eq a => EqList Label a -> EqList Label a -> Bool
equals (xs, lx) (ys, ly) =
      lx == ly &&
      length xs == length ys &&
      all (\x -> any (\y -> x == y) ys) xs &&
      all (\y -> any (\x -> x == y) xs) ys

type Label = Integer

nextLabel :: Label -> Label
nextLabel = (+1)

-- sigle step
buildEqSingleton :: a -> Label -> EqList Label a
buildEqSingleton x l = ([(l, x, nextLabel l)], nextLabel l)
