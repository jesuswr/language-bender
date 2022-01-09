element pto is compound by x skill of air, y skill of air.

travel add made by pto bender a, pto bender b, pto reincarnation bender c:
.-
    c's x is ((using a's x skill) and then (using b's x skill)).
    c's y is ((using a's y skill) and then (using b's y skill))~
-.

travel sub made by pto bender a, pto bender b, pto reincarnation bender c:
.-
    c's x is ((using a's x skill) but (using b's x skill)).
    c's y is ((using a's y skill) but (using b's y skill))~
-.

book dot_product of air about pto bender a, pto bender b:
.-
    ((using a's x skill) and thus (using b's x skill))
    and then
    ((using a's y skill) and thus (using b's y skill))
-.

book cross_product of air about pto bender a, pto bender b:
.-
    ((using a's x skill) and thus (using b's y skill))
    but
    ((using a's y skill) and thus (using b's x skill))
-.

-- checks if point a is left to line q->r
book is_left of fire about pto bender a, pto bender q, pto bender r:
.-
    bender tmp0 of pto.
    bender tmp1 of pto.
    in sub travel with q, a, tmp0....
    in sub travel with r, a, tmp1....
    (in cross_product book with tmp0, tmp1...) is greater than 0
-.



travel main:
.-
    bender p1 of pto.
    bender p2 of pto.
    bender p3 of pto.

    in printmetal travel with "DE VALORES PARA EL PRIMER PUNTO"....
    in readair travel with (using p1's x skill)....
    in readair travel with (using p1's y skill)....
    in printmetal travel with "DE VALORES PARA EL SEGUNDO PUNTO"....
    in readair travel with (using p2's x skill)....
    in readair travel with (using p2's y skill)....

    bender opt of air.
    in printmetal travel with "1 PARA SUMAR, 2 PARA RESTAR, 3 PARA PRODUCTO PUNTO, 4 PARA PRODUCTO CRUZ, 5 PARA VER SI UN PUNTO ESTA A LA IZQUIERDA DE LA RECTA DEL PRIMER AL SEGUNDO PUNTO"....
    in readair travel with opt....

    if (opt is equal to 1):
    .-
        in add travel with p1, p2, p3....
        in printair travel with (using p3's x skill)....
        in printair travel with (using p3's y skill)...
    -.
    otherwise if (opt is equal to 2):
    .-
        in sub travel with p1, p2, p3....
        in printair travel with (using p3's x skill)....
        in printair travel with (using p3's y skill)...
    -.
    otherwise if (opt is equal to 3):
    .-
        in printair travel with (in dot_product book with p1, p2...)...
    -.
    otherwise if (opt is equal to 4):
    .-
        in printair travel with (in cross_product book with p1, p2...)...
    -.
    otherwise if (opt is equal to 5):
    .-
        in printmetal travel with "DE VALORES PARA EL TERCER PUNTO"....
        in readair travel with (using p3's x skill)....
        in readair travel with (using p3's y skill)....
        in printfire travel with (in is_left book with p3, p1, p2...)...
    -.

-.