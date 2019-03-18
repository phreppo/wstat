Cose che faremo

    . lasciare cosi instance ASD SD Var SimpleSignDomain essendo consapevoli che faccia schifo
        -> fare il dominio dei segni vero (SignDomain)
            -> fare instance ASD SD Var SignDomain
                -> test SignDomain
    . pretty printer

    . fare dominio degli intervalli (IntervalDomain) come AVD
        -> rendere SD Var IntervalDomain istanza di ASD
            -> test && provare il calcolatore di widening points

    . la label la facciamo iniziare da dove le pare, ma tipo nel calcolo del punto fisso facciamo assunzioni sul fatto
      che se sia la 1 allora la lasciamo uguale, questo non va bene

    . parametrizzare in modo intelligente il buildInitialState che permetta di selezionare il tipo di ASD

Cose che faremo alla fine
    . readme fico
    . controllare che tutti gli import e quello che si espone sia minimale e non ridondante

Cose che probabilmente non faremo alla fine
    . bottom top meet join etc simbolici

Cose che non faremo
    . more precise way to do backward avd operators
    . travis


[Freepo]
    . capire che fanno atomic assign e cond -> rifattorizzare i meno moduli
[Danses]
    .