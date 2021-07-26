book notInScope:
.-
    bender a1 is b1.
-.

book redef:
.-
    bender a2 is 1.
    bender a2 is "hola".
-.

book refNonVar:
.-
    bender a3 is reincarnation of redef.
-.

book f of air about air bender aang: aang. 

book notValidFunc:
.-
    energy b4 allows b40 technique of air bending.
    bender a4 is in b4 book with 1 ...
-.

book notArray of air:
.-
    bender b5 of air.
    disciple 1 of b5.
-.

book unMatchingTypes:
.-
    bender a6 is "hola".
    1 and then a6.
-.

book fib of air about air bender a7, air bender b7, air bender c7: 1.

book badNumArgs:
.-
    bender y8 is in fib book with 0,1,2,3,4 ....  
-.

book badNumArgs2:
.-
    bender y9 is in fib book with 0,1 ....
-.


travel fib2 made by air bender a7, air bender b7, air bender c7 is 1:
.-
-.


book badNumArgs3:
.-
    fib2 travel with 0,1,2,3,4 ....  
-.

book badNumArgs4:
.-
    fib2 travel with 0,1,2 ....
-.