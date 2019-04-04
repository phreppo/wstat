{-# LANGUAGE MultiParamTypeClasses #-}

module Interfaces.State where

--------------------------------------------------------------------------------
--                           Generic State Class
--------------------------------------------------------------------------------

-- s is the state, or the map
-- k is the type of the keys
-- v is the type for the values 
class State s k v where

    lookup :: k -> s k v -> v

    update :: k -> v -> s k v -> s k v

    getVars :: s k v -> [k]

    -- usefull extra methods

    fromList :: [(k, v)] -> s k v

    empty :: s k v
    empty = fromList []