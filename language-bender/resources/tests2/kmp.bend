travel kmp:
.-
    -- input
    in printmetal travel with "DE EL TAMANO DEL PATRON"....
    bender n of air.
    in readair travel with n....

    in printmetal travel with "DE EL PATRON"....
    bender pattern of metal with (n and then 1) purity.
    in readmetal travel with pattern....
    disciple n of pattern really is '$'.

    in printmetal travel with "DE EL TAMANO DEL TEXTO"....
    bender m of air.
    in readair travel with m....

    in printmetal travel with "DE EL TEXTO"....
    bender text of metal with m purity.
    in readmetal travel with text....

    -- build prefix function table
    bender pi of air nation since (n and then 1) years.
    disciple 0 of pi really is 0.
    disciple n of pi really is 0.

    opening 1 of i chakras from 1 to n:
    .-
        bender j of air is disciple (i but 1) of pi.
        while ((j is greater than 0) and  ((disciple i of pattern) is not equal to (disciple j of pattern))) doing:
        .-
            j is disciple (j but 1) of pi.
        -.
        if ((disciple i of pattern) is equal to (disciple j of pattern)):
            disciple i of pi really is (j and then 1).
        otherwise
            disciple i of pi really is 0.
    -.

    -- use table to get matches
    bender last is 0.
    bender i is 0.
    opening 1 of i chakras from 0 to m:
    .-
        bender j is last.
        bender curr is 0.
        while ((j is greater than 0) and ((disciple i of text) is not equal to (disciple j of pattern))) doing:
        .-
            j is disciple (j but 1) of pi.
        -.
        if ((disciple i of text) is equal to (disciple j of pattern)):
            curr is (j and then 1).
        otherwise
            curr is 0.

        last is curr.
        i is i and then 1.
        if (curr is equal to n):
        .-
            in printmetal book with "HAY MATCH EN LA POSICION:"....
            in printair book with (i but n)...
        -.
    -.
-.

travel main:
.-
    in kmp travel with...
-.