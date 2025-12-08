import HouLean

open HouLean Apex Compiler

open Qq

-- #guard_msgs in
-- #apex_graph fun x : struct {a : Float} => x.a

-- #guard_msgs in
-- #apex_graph fun x : struct {a : Float, b : Float, c : Float, d : Float} => x.b

-- #guard_msgs in
-- #apex_graph fun x : struct {a : Float, b : Float, c : Float, d : Float} => x.c

-- #guard_msgs in
-- #apex_graph fun x : struct {a : Float, b : Float, c : Float, d : Float} => x.d
