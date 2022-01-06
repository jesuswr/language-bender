book rec_fib of air about air bender n:
.-
    if (n is equal to 0): 
        0
    otherwise if (n is equal to 1): 
        1
    otherwise 
        (in rec_fib book with n but 1 ...) 
            and then 
        (in rec_fib book with n but 2 ...)
-.

travel main:
.-
    while lightning master doing:
    .-
        in printmetal travel with "DE 0 PARA TERMINAR O 1 PARA FIB REC"....
        bender finish of air.
        in readair travel with finish....
        if (finish is equal to 0):
            burst ~
        otherwise
        .-
            in printmetal travel with "INTRODUZCA UN NUMERO MAYOR O IGUAL QUE 0"....
            bender n of air.
            in readair travel with n....
            in printair travel with (in rec_fib book with n ...)...
        -.
    -.
-.