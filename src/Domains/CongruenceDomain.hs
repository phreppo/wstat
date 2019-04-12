module Domains.CongruenceDomain where

--

import SyntacticStructure.WhileGrammar
import Tools.Utilities

data CongruenceDomain = Congruence I I
                      | BottomCongruence
                      deriving (Read, Eq, Ord)

instance Show CongruenceDomain where
    show BottomCongruence = bottomString
    show (Congruence a b) = "∊ " ++ show a ++ "ℤ + " ++ show b

--------------------------------------------------------------------------------
--                             Utility Functions
--------------------------------------------------------------------------------

divides :: Integer -> Integer -> Bool
y `divides` y' = y' `mod` y == 0
-- y || y' = any id [y' == k * y | k <- [0..]]

isCongruence :: Integer -> Integer -> Integer -> Bool
isCongruence x x' y = y `divides` (makePositive $ x - x')

makePositive :: Integer -> Integer
makePositive x | x >= 0 = x
               | otherwise = -x

lcmWithZero :: Integer -> Integer -> Integer
lcmWithZero 0 _  = 0
lcmWithZero _ 0  = 0
lcmWithZero y y' = lcm y y'

gcdZeroTransparent :: Integer -> Integer -> Integer
gcdZeroTransparent 0 y  = y
gcdZeroTransparent y 0  = y
gcdZeroTransparent y y' = gcd y y'