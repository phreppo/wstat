{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Domains.SignDomain where

--------------------------------------------------------------------------------
--                             Sign Domain
--------------------------------------------------------------------------------

data SignDomain = BottomSign
                | NonZero
                | EqualZero
                | GreaterZero
                | GreaterEqZero
                | LowerZero
                | LowerEqZero
                | TopSign
                deriving (Show, Read, Eq, Ord, Enum)