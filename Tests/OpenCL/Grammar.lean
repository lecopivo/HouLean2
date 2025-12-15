import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Compiler.Grammar


namespace Test.OpenCL.Grammar

open HouLean OpenCL

implemented_by {α} [Inhabited α] [AtomicOpenCLType α] [Add α] (x y : α) : x + y = ocl%( x + y )
implemented_by {α} [Inhabited α] [AtomicOpenCLType α] [Sub α] (x y : α) : x - y = ocl%( x - y )
implemented_by {α} [Inhabited α] [AtomicOpenCLType α] [Mul α] (x y : α) : x * y = ocl%( x * y )
implemented_by {α} [Inhabited α] [AtomicOpenCLType α] [Div α] (x y : α) : x / y = ocl%( x / y )
implemented_by {α} [Inhabited α] [AtomicOpenCLType α] [Neg α] (x : α) : -x = ocl%( - x )


variable (x y : Float)


/--
info: fun x y =>
  have a := ocl%(x + y * x / y - x);
  ocl%(a + x) : Float → Float → Float
-/
#guard_msgs in
#check (fun x y : Float => let a := (x + y * x) / y - x; a + x)
  rewrite_by
    simp -zeta only [opencl_csimp]

attribute [opencl_csimp] normalize_ocl_fun_name


/-- info: fun x y => #v[x, y, x] : Float → Float → Vector Float 3 -/
#guard_msgs in
#check (fun x y : Float => #v[x,y,x])
  rewrite_by
    simp -zeta only [opencl_csimp]


/-- info: fun x y => #v[x, y, x][1] : Float → Float → Float -/
#guard_msgs in
#check (fun x y : Float => #v[x,y,x][1])
  rewrite_by
    simp -zeta only [opencl_csimp]


/-- info: fun x y => ({ data := #v[#v[x, y], #v[y, x]] }.row 0 ⋯)[0] : Float → Float → Float -/
#guard_msgs in
#check (fun x y : Float => #m[#v[x,y],#v[y,x]][(0,0)])
  rewrite_by
    simp -zeta only [opencl_csimp]
