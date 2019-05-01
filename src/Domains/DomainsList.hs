module Domains.DomainsList where

simpleSign = "ss"
sign = "s"
interval = "i"
congruence = "c"
reductionIntervalCongruence = "ric"

listAllDomains =
    simpleSign ++ ", " ++
    sign ++ ", " ++
    interval
    -- congruence ++ ", " ++
    -- reductionIntervalCongruence