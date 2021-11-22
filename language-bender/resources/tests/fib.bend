book rec_fib of air about air bender n:
.-
    if (n is equal to 0): 
        0
    otherwise if (n is equal to 1): 
        1
    otherwise 
        (in rec_fib book with n-1 ...) 
            and then 
        (in rec_fib book with n-2 ...)
-.

book it_fib of air about air bender n:
.-
    bender dp of air is 0.
    bender next_dp of air is 1.
    opening 1 of _ chakras from 0 to n:
    .-
        next_dp is dp and then next_dp. -- f(n+1) = f(n-1) + f(n)
        dp is next_dp but dp            -- f(n) = f(n+1) - f(n-1)
    -.
-.

travel main:
.-
    while lightning master doing:
    .-
        -- PRINT("DE 0 PARA TERMINAR, 1 PARA FIB REC O 2 PARA FIB ITER")
        bender finish of air.
        -- READ(finish)
        if (finish is equal to 0):
            burst ~
        otherwise
        .-
            -- PRINT("INTRODUZCA UN NUMERO MAYOR O IGUAL QUE 0")
            bender n of air.
            -- READ(n)
            if (finish is equal to 1):
                -- PRINT(in book rec_fib with n ...)
                to be continued ~
            otherwise
                -- PRINT(in book it_fib with n ...)
                to be continued ~
        -.
    -.
-.