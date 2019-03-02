{-# LANGUAGE MultiParamTypeClasses #-}

module Interfaces.State where

--------------------------------------------------------------------------------
-- generic interface for State
--------------------------------------------------------------------------------

class State s k v where

    lookup :: k -> s k v -> v

    update :: k -> v -> s k v -> s k v

    -- usefull extra methods

    fromList :: [(k, v)] -> s k v

    empty :: s k v
    empty = fromList []