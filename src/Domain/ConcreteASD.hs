module Domain.ConcreteASD where

import Domain.ASD
import qualified Data.Map.Lazy as Map
    
data SD = S Map
        | Bottom

-- instance ASD