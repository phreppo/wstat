Cose che faremo alla fine
    . readme fico
    . controllare che tutti gli import e quello che si espone sia minimale e non ridondante

Cose che probabilmente non faremo alla fine
    . bottom top meet join etc simbolici
    . code coverage

Cose che non faremo
    . more precise way to do backward avd operators


[Freepo]
    . capire che fanno atomic assign e cond -> rifattorizzare i meno moduli
    . fare dominio degli intervalli (IntervalDomain) come AVD
        -> rendere SD Var IntervalDomain istanza di ASD
            -> test && provare il calcolatore di widening points
    . alcuni esempi come la funzione get graph di programmi e dove metterli nel codice (ciclo ke nn eske mai mai lui eske xd)
    . parser di mappe var-abstract value per per AVD
        -> meet fra quello che viene passato e quello che cie gia build

[Danses]
    . lasciare cosi instance ASD SD Var SimpleSignDomain essendo consapevoli che faccia schifo
        -> fare il dominio dei segni vero (SignDomain)
            -> fare instance ASD SD Var SignDomain
                -> test SignDomain
