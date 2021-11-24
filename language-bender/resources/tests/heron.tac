goto main@0
@label heron@0
@function 8
assign T0 1.0
add x@T1 BASE 0
# Copy 4 bytes from "T0" to "x@T1"
assign T3 0
assign T4 50
assign T5 1
lderef BASE 4 T3
@label for_start@L1
rderef _@T7 BASE 4
geq T6 _@T7 T4
goif for_end@L3 T6
assign T8 0.5
add x@T9 BASE 0
rderef T10 x@T9 0
add s@T11 BASE 0
rderef T12 s@T11 0
add x@T13 BASE 0
rderef T14 x@T13 0
div T15 T12 T14
add T16 T10 T15
mult T17 T8 T16
add x@T18 BASE 0
# Copy 4 bytes from "T17" to "x@T18"
@label for_step@L2
add _@T7 _@T7 T5
lderef BASE 4 _@T7
assign for_result@T2 x@T18
goto for_start@L1
@label for_end@L3
return for_result@T2
@label heron_end@L0
@endfunction 8
@label main@0
@function 8
@label while_start@L5
assign T20 True
neg T20 T20
goif while_out@L6 T20
call freturn@T21 readair@11 0
add finish@T22 BASE 0
# Copy 4 bytes from "freturn@T21" to "finish@T22"
add finish@T24 BASE 0
rderef T25 finish@T24 0
assign T26 0
eq T27 T25 T26
neg T28 T27
goif if_else@L7 T28
goto while_out@L6
goto if_out@L8
@label if_else@L7
call freturn@T29 readair@14 0
add n@T30 BASE 4
# Copy 4 bytes from "freturn@T29" to "n@T30"
add n@T31 BASE 4
rderef T32 n@T31 0
param T32
call freturn@T33 heron@14 1
param freturn@T33
call freturn@T34 printwater@14 1
@label if_out@L8
goto while_start@L5
@label while_out@L6
@label main_end@L4
@endfunction 8
@label IMPLEMENTACION DE PRINTS
@label printair@0
@function 4
@label TODO
@label printair_end@L9
@endfunction 4
@label printwater@0
@function 4
@label TODO
@label printwater_end@L10
@endfunction 4
@label printfire@0
@function 4
@label TODO
@label printfire_end@L11
@endfunction 4
@label printearth@0
@function 4
@label TODO
@label printearth_end@L12
@endfunction 4
@label printmetal@0
@function 8
@label TODO
@label printmetal_end@L13
@endfunction 8
@label IMPLEMENTACION DE READS
@label readair@0
@function 0
@label TODO
@label readair_end@L14
@endfunction 0
@label readwater@0
@function 0
@label TODO
@label readwater_end@L15
@endfunction 0
@label readfire@0
@function 0
@label TODO
@label readfire_end@L16
@endfunction 0
@label readearth@0
@function 0
@label TODO
@label readearth_end@L17
@endfunction 0
@label readmetal@0
@function 4
@label TODO
@label readmetal_end@L18
@endfunction 4

