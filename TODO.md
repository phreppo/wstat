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
    . Intervalli
        -> fare per intero la cond su variabile e variabile
        -> fare per intero la cond su costante e costante
        -> fare per intero la cond su eq e not eq
        -> _opzionale_ fare per interno la cond su costante e variabile, probabilmente basta invertire robe

[Danses]
    . lasciare cosi instance ASD SD Var SimpleSignDomain essendo consapevoli che faccia schifo
        -> fare il dominio dei segni vero (SignDomain)
            -> fare instance ASD SD Var SignDomain
                -> test SignDomain
