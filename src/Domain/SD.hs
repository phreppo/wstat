module Domain.SD where

import Data.Map

data SD b = SD (Map String b)
          | Bottom
          deriving (Show)
