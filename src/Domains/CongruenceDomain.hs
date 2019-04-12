module Domains.CongruenceDomain where

--

import SyntacticStructure.WhileGrammar
import Tools.Utilities
import Interfaces.AbstractDomain

data CongruenceDomain = Congruence I I
                      | BottomCongruence
                      deriving (Read, Eq, Ord)

instance Show CongruenceDomain where
    show BottomCongruence = bottomString
    show (Congruence 1 0) = "∊ ⊤"
    show (Congruence a b) = "∊ " ++ show a ++ "ℤ + " ++ show b

instance AbstractDomain CongruenceDomain where

    subset BottomCongruence _ = True
    subset _ BottomCongruence = False
    subset (Congruence a b) (Congruence a' b') =
        a' `divides` a && isCongruence b b' a'

    join BottomCongruence x = x
    join x BottomCongruence = x
    join (Congruence a b) (Congruence a' b') = Congruence newA b
        where newA = a ∧ a' ∧ makePositive (b - b')

    meet BottomCongruence x = BottomCongruence
    meet x BottomCongruence = BottomCongruence
    meet (Congruence a b) (Congruence a' b')
        | isCongruence b b' (a ∧ a') = Congruence (a ∨ a') b''
        | otherwise = BottomCongruence
      where b'' = greedySearch b b' (a ∨ a')
        -- TODO: search for a better implementation using extended gcd algorithm

    bottom = BottomCongruence

    top = Congruence 1 0

    -- Although the domain has an infinite height,
    -- there is no infinite strictly increasing chain.
    widen = join

    narrow BottomCongruence _ = BottomCongruence
    narrow _ BottomCongruence = BottomCongruence
    narrow (Congruence 1 _) y = y
    narrow x                _ = x



--------------------------------------------------------------------------------
--                             Utility Functions
--------------------------------------------------------------------------------

divides :: Integer -> Integer -> Bool
y `divides` y' = y' `mod` y == 0
-- y || y' = any id [y' == k * y | k <- [0..]]

isCongruence :: Integer -> Integer -> Integer -> Bool
isCongruence x x' 0 = x == x'
isCongruence x x' y = y `divides` (makePositive $ x - x')

makePositive :: Integer -> Integer
makePositive x | x >= 0 = x
               | otherwise = -x

(∨) :: Integer -> Integer -> Integer
(∨) 0 _  = 0
(∨) _ 0  = 0
(∨) y y' = lcm y y'

(∧) :: Integer -> Integer -> Integer
(∧) 0 y  = y
(∧) y 0  = y
(∧) y y' = gcd y y'

bzIdentity :: Integer -> Integer -> Integer
bzIdentity a b = fst $ extendedGcd a b

-- extended euclidean algorithm to find bezout's identity
extendedGcd ::
    Integer -> -- a
    Integer -> -- b
    (Integer, Integer) -- (bezout's identity, gcd)
extendedGcd a 0 = (1, 0)
extendedGcd a b = (t, s - q * t)
    where (q, r) = quotRem a b
          (s, t) = extendedGcd b r

greedySearch :: Integer -> Integer -> Integer -> Integer
greedySearch b b' a'' = let Just x = filterFirst cond [0..] in x
    where cond b'' = isCongruence b'' b a'' && isCongruence b'' b' a''

filterFirst :: (a -> Bool) -> [a] -> Maybe a
filterFirst _ [] = Nothing
filterFirst p (x:xs)
    | p x       = Just x
    | otherwise = filterFirst p xs