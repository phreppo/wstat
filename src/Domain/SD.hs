module Domain.SD where

import Data.Map

data SD v b = SD (Map v b)
            | Bottom
            deriving (Show)
