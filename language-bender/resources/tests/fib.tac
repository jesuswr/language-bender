goto main@0
@label rec_fib@0
@function 0
add n@T1 BASE 0
rderef T2 n@T1 0
assign T3 0
eq T4 T2 T3
neg T5 T4
goif if_else@L1 T5
assign T6 0
assign T0 T6
goto if_out@L2
@label if_else@L1
add n@T8 BASE 0
rderef T9 n@T8 0
assign T10 1
eq T11 T9 T10
neg T12 T11
goif if_else@L3 T12
assign T13 1
assign T7 T13
goto if_out@L4
@label if_else@L3
add n@T14 BASE 0
rderef T15 n@T14 0
assign T16 1
sub T17 T15 T16
param T17
call freturn@T18 rec_fib@7 1
add n@T19 BASE 0
rderef T20 n@T19 0
assign T21 2
sub T22 T20 T21
param T22
call freturn@T23 rec_fib@7 1
add T24 freturn@T18 freturn@T23
assign T7 T24
@label if_out@L4
assign T0 T7
@label if_out@L2
return T0
@label rec_fib_end@L0
@endfunction 0
@label it_fib@0
@function 12
assign T25 0
add dp@T26 BASE 0
# Copy 4 bytes from "T25" to "dp@T26"
assign T27 1
add next_dp@T28 BASE 4
# Copy 4 bytes from "T27" to "next_dp@T28"
assign T30 0
add n@T31 BASE 0
rderef T32 n@T31 0
assign T33 1
lderef BASE 8 T30
@label for_start@L6
rderef _@T35 BASE 8
geq T34 _@T35 T32
goif for_end@L8 T34
add dp@T36 BASE 0
rderef T37 dp@T36 0
add next_dp@T38 BASE 4
rderef T39 next_dp@T38 0
add T40 T37 T39
add next_dp@T41 BASE 4
# Copy 4 bytes from "T40" to "next_dp@T41"
add next_dp@T42 BASE 4
rderef T43 next_dp@T42 0
add dp@T44 BASE 0
rderef T45 dp@T44 0
sub T46 T43 T45
add dp@T47 BASE 0
# Copy 4 bytes from "T46" to "dp@T47"
@label for_step@L7
add _@T35 _@T35 T33
lderef BASE 8 _@T35
assign for_result@T29 dp@T47
goto for_start@L6
@label for_end@L8
return for_result@T29
@label it_fib_end@L5
@endfunction 12
@label main@0
@function 8
@label while_start@L10
assign T49 True
neg T49 T49
goif while_out@L11 T49
call freturn@T50 readair@18 0
add finish@T51 BASE 0
# Copy 4 bytes from "freturn@T50" to "finish@T51"
add finish@T53 BASE 0
rderef T54 finish@T53 0
assign T55 0
eq T56 T54 T55
neg T57 T56
goif if_else@L12 T57
goto while_out@L11
goto if_out@L13
@label if_else@L12
call freturn@T58 readair@21 0
add n@T59 BASE 4
# Copy 4 bytes from "freturn@T58" to "n@T59"
add finish@T61 BASE 0
rderef T62 finish@T61 0
assign T63 1
eq T64 T62 T63
neg T65 T64
goif if_else@L14 T65
add n@T66 BASE 4
rderef T67 n@T66 0
param T67
call freturn@T68 rec_fib@22 1
param freturn@T68
call freturn@T69 printair@22 1
goto if_out@L15
@label if_else@L14
add n@T70 BASE 4
rderef T71 n@T70 0
param T71
call freturn@T72 it_fib@23 1
param freturn@T72
call freturn@T73 printair@23 1
@label if_out@L15
@label if_out@L13
goto while_start@L10
@label while_out@L11
@label main_end@L9
@endfunction 8
@label IMPLEMENTACION DE PRINTS
@label printair@0
@function 4
@label TODO
@label printair_end@L16
@endfunction 4
@label printwater@0
@function 4
@label TODO
@label printwater_end@L17
@endfunction 4
@label printfire@0
@function 4
@label TODO
@label printfire_end@L18
@endfunction 4
@label printearth@0
@function 4
@label TODO
@label printearth_end@L19
@endfunction 4
@label printmetal@0
@function 8
@label TODO
@label printmetal_end@L20
@endfunction 8
@label IMPLEMENTACION DE READS
@label readair@0
@function 0
@label TODO
@label readair_end@L21
@endfunction 0
@label readwater@0
@function 0
@label TODO
@label readwater_end@L22
@endfunction 0
@label readfire@0
@function 0
@label TODO
@label readfire_end@L23
@endfunction 0
@label readearth@0
@function 0
@label TODO
@label readearth_end@L24
@endfunction 0
@label readmetal@0
@function 4
@label TODO
@label readmetal_end@L25
@endfunction 4

