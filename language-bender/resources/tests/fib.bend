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
    opening 1 of leonardo_pisano chakras from 0 to n:
    .-
        next_dp is dp and then next_dp. -- f(n+1) = f(n-1) + f(n)
        dp is next_dp but dp            -- f(n) = f(n+1) - f(n-1)
    -.
-.