goto main@0
@label par@0
@function 0
add n@T1 BASE 0
rderef T2 n@T1 0
assign T3 0
eq T4 T2 T3
neg T5 T4
goif if_else@L1 T5
assign T6 True
assign T0 T6
goto if_out@L2
@label if_else@L1
add n@T8 BASE 0
rderef T9 n@T8 0
assign T10 1
eq T11 T9 T10
neg T12 T11
goif if_else@L3 T12
assign T13 False
assign T7 T13
goto if_out@L4
@label if_else@L3
add n@T14 BASE 0
rderef T15 n@T14 0
assign T16 1
sub T17 T15 T16
param T17
call freturn@T18 impar@7 1
assign T7 freturn@T18
@label if_out@L4
assign T0 T7
@label if_out@L2
return T0
@label par_end@L0
@endfunction 0
@label impar@0
@function 0
add n@T20 BASE 0
rderef T21 n@T20 0
assign T22 0
eq T23 T21 T22
neg T24 T23
goif if_else@L6 T24
assign T25 False
assign T19 T25
goto if_out@L7
@label if_else@L6
add n@T27 BASE 0
rderef T28 n@T27 0
assign T29 1
eq T30 T28 T29
neg T31 T30
goif if_else@L8 T31
assign T32 True
assign T26 T32
goto if_out@L9
@label if_else@L8
add n@T33 BASE 0
rderef T34 n@T33 0
assign T35 1
sub T36 T34 T35
param T36
call freturn@T37 par@14 1
assign T26 freturn@T37
@label if_out@L9
assign T19 T26
@label if_out@L7
return T19
@label impar_end@L5
@endfunction 0
@label main@0
@function 8
@label while_start@L11
assign T39 True
neg T39 T39
goif while_out@L12 T39
call freturn@T40 readair@19 0
add finish@T41 BASE 0
# Copy 4 bytes from "freturn@T40" to "finish@T41"
add finish@T43 BASE 0
rderef T44 finish@T43 0
assign T45 0
eq T46 T44 T45
neg T47 T46
goif if_else@L13 T47
goto while_out@L12
goto if_out@L14
@label if_else@L13
call freturn@T48 readair@22 0
add n@T49 BASE 4
# Copy 4 bytes from "freturn@T48" to "n@T49"
add n@T51 BASE 4
rderef T52 n@T51 0
param T52
call freturn@T53 par@22 1
neg T54 freturn@T53
goif if_else@L15 T54
goto while_start@L11
goto if_out@L16
@label if_else@L15
goto while_start@L11
@label if_out@L16
@label if_out@L14
goto while_start@L11
@label while_out@L12
@label main_end@L10
@endfunction 8
@label IMPLEMENTACION DE PRINTS
@label printair@0
@function 4
@label TODO
@label printair_end@L17
@endfunction 4
@label printwater@0
@function 4
@label TODO
@label printwater_end@L18
@endfunction 4
@label printfire@0
@function 4
@label TODO
@label printfire_end@L19
@endfunction 4
@label printearth@0
@function 4
@label TODO
@label printearth_end@L20
@endfunction 4
@label printmetal@0
@function 8
@label TODO
@label printmetal_end@L21
@endfunction 8
@label IMPLEMENTACION DE READS
@label readair@0
@function 0
@label TODO
@label readair_end@L22
@endfunction 0
@label readwater@0
@function 0
@label TODO
@label readwater_end@L23
@endfunction 0
@label readfire@0
@function 0
@label TODO
@label readfire_end@L24
@endfunction 0
@label readearth@0
@function 0
@label TODO
@label readearth_end@L25
@endfunction 0
@label readmetal@0
@function 4
@label TODO
@label readmetal_end@L26
@endfunction 4

