module Equation.EquationList where

--------------------------------------------------------------------------------
-- Equation Abstract Data Type
--------------------------------------------------------------------------------

data Equation l a = Equation (l, a, l) deriving (Show, Eq)

-- TODO: make this a set
-- X list
data EqList l a = EqList ([Equation l a], l) deriving Show

getCtx :: Label l => Equation l a -> a
getCtx (Equation (_, x, _)) = x

instance (Label l, Eq a) => Eq (EqList l a) where
  EqList (xs, lx) == EqList (ys, ly) =
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
buildEqSingleton x l = EqList ([Equation (l, x, nextLabel l)], nextLabel l)
