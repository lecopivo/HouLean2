import HouLean.OpenCL.Compiler.Main

namespace HouLean.OpenCL

attribute [opencl_csimp] sum

implemented_by (f : α → Fin 0 → α) (init : α) : Fin.foldl 0 f init = init
implemented_by (f : α → Fin 1 → α) (init : α) : Fin.foldl 1 f init = f init 0
implemented_by (f : α → Fin 2 → α) (init : α) : Fin.foldl 2 f init = f (f init 0) 1
implemented_by (f : α → Fin 3 → α) (init : α) : Fin.foldl 3 f init = f (f (f init 0) 1) 2
implemented_by (f : α → Fin 4 → α) (init : α) : Fin.foldl 4 f init = f (f (f (f init 0) 1) 2) 3
implemented_by (f : α → Fin 5 → α) (init : α) : Fin.foldl 5 f init = f (f (f (f (f init 0) 1) 2) 3) 4
implemented_by (f : α → Fin 6 → α) (init : α) : Fin.foldl 6 f init = f (f (f (f (f (f init 0) 1) 2) 3) 4) 5
implemented_by (f : α → Fin 7 → α) (init : α) : Fin.foldl 7 f init = f (f (f (f (f (f (f init 0) 1) 2) 3) 4) 5) 6
implemented_by (f : α → Fin 8 → α) (init : α) : Fin.foldl 8 f init = f (f (f (f (f (f (f (f init 0) 1) 2) 3) 4) 5) 6) 7
implemented_by (f : α → Fin 9 → α) (init : α) : Fin.foldl 9 f init = f (f (f (f (f (f (f (f (f init 0) 1) 2) 3) 4) 5) 6) 7) 8
implemented_by (f : α → Fin 10 → α) (init : α) : Fin.foldl 10 f init = f (f (f (f (f (f (f (f (f (f init 0) 1) 2) 3) 4) 5) 6) 7) 8) 9

implemented_by (f : Fin 0 → α → α) (init : α) : Fin.foldr 0 f init = init
implemented_by (f : Fin 1 → α → α) (init : α) : Fin.foldr 1 f init = f 0 init
implemented_by (f : Fin 2 → α → α) (init : α) : Fin.foldr 2 f init = f 1 (f 0 init)
implemented_by (f : Fin 3 → α → α) (init : α) : Fin.foldr 3 f init = f 2 (f 1 (f 0 init))
implemented_by (f : Fin 4 → α → α) (init : α) : Fin.foldr 4 f init = f 3 (f 2 (f 1 (f 0 init)))
implemented_by (f : Fin 5 → α → α) (init : α) : Fin.foldr 5 f init = f 4 (f 3 (f 2 (f 1 (f 0 init))))
implemented_by (f : Fin 6 → α → α) (init : α) : Fin.foldr 6 f init = f 5 (f 4 (f 3 (f 2 (f 1 (f 0 init)))))
implemented_by (f : Fin 7 → α → α) (init : α) : Fin.foldr 7 f init = f 6 (f 5 (f 4 (f 3 (f 2 (f 1 (f 0 init))))))
implemented_by (f : Fin 8 → α → α) (init : α) : Fin.foldr 8 f init = f 7 (f 6 (f 5 (f 4 (f 3 (f 2 (f 1 (f 0 init)))))))
implemented_by (f : Fin 9 → α → α) (init : α) : Fin.foldr 9 f init = f 8 (f 7 (f 6 (f 5 (f 4 (f 3 (f 2 (f 1 (f 0 init))))))))
implemented_by (f : Fin 10 → α → α) (init : α) : Fin.foldr 10 f init = f 9 (f 8 (f 7 (f 6 (f 5 (f 4 (f 3 (f 2 (f 1 (f 0 init)))))))))
