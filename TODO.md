Cose che faremo alla fine
    . readme fico
    . controllare che tutti gli import e quello che si espone sia minimale e non ridondante
    . backward analysis

Cose che probabilmente non faremo alla fine
    . code coverage

Cose che non faremo
    . more precise way to do backward avd operators

Bug:
    . non ci dovrebbero mai essere dipendenze a Data.Map ma solo a Interfaces.State, forse bisognerà aggiungere il metodo fromListLazy che non butta a Bottom le liste vuote
