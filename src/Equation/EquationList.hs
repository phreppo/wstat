module Equation.EquationList where

--------------------------------------------------------------------------------
-- Equation Abstract Data Type
--------------------------------------------------------------------------------

type Equation l a = (l, a, l)

-- TODO: make this a set
-- X list
type EqList l a = ([Equation l a], l)

getCtx :: Label l => Equation l a -> a
getCtx (_, x, _) = x

equals :: (Label l, Eq a) => EqList l a -> EqList l a -> Bool
equals (xs, lx) (ys, ly) =
      lx == ly &&
      length xs == length ys &&
      all (\x -> any (\y -> x == y) ys) xs &&
      all (\y -> any (\x -> x == y) xs) ys

class Eq a => Label a where
  nextLabel :: a -> a

instance Label Integer where
  nextLabel = (+1)

-- sigle step
buildEqSingleton :: Label l => a -> l -> EqList l a
buildEqSingleton x l = ([(l, x, nextLabel l)], nextLabel l)
