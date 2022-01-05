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
    bender n of air.
    in readair travel with n....
    bender x is rec_fib book with n....
    in printair travel with x...
-.