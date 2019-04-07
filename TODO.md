Cose che faremo alla fine
    . readme fico
    . controllare che tutti gli import e quello che si espone sia minimale e non ridondante
    . backward analysis

Cose che probabilmente non faremo alla fine
    . code coverage

Cose che non faremo
    . more precise way to do backward avd operators

[Freepo]
    . capire che fanno atomic assign e cond -> rifattorizzare i meno moduli

[Danses]
    . lasciare cosi instance AbstractStateDomain RelationalStateDomain Var SimpleSignDomain essendo consapevoli che faccia schifo
        -> fare il dominio dei segni vero (SignDomain)
            -> fare instance AbstractStateDomain RelationalStateDomain Var SignDomain
                -> test SignDomain
