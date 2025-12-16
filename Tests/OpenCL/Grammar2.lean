/-
OpenCL Parser Check Commands and Tests
-/
import HouLean.OpenCL.Compiler.Grammar2
import Lean

open Lean Parser Elab Command Meta

namespace HouLean.OpenCL

--------------------------------------------------------------------------------
/-! ## Check Commands                                                         -/
--------------------------------------------------------------------------------

elab c:"#opencl_check_def " e:clCmd : command => do
  logInfoAt c e.raw.prettyPrint

elab c:"#opencl_check_def " e:clDeclaration : command => do
  logInfoAt c e.raw.prettyPrint

elab c:"#opencl_check_type " e:clType : command => do
  logInfoAt c e

elab c:"#opencl_check_expr " e:clExpr : command => do
  logInfoAt c e.raw.prettyPrint

elab c:"#opencl_check " e:clCompStmt : command => do
  logInfoAt c e.raw.prettyPrint

--------------------------------------------------------------------------------
/-! ## Basic Declaration Tests                                                -/
--------------------------------------------------------------------------------

section DeclTests

-- Simple variable declarations
/-- info: int x; -/
#guard_msgs in
#opencl_check_def int x;

/-- info: float y; -/
#guard_msgs in
#opencl_check_def float y;

/-- info: double z; -/
#guard_msgs in
#opencl_check_def double z;

-- With initialization
/-- info: int x = 0; -/
#guard_msgs in
#opencl_check_def int x = 0;

/-- info: float y = 1.0; -/
#guard_msgs in
#opencl_check_def float y = 1.0;

/-- info: int a = 1, b = 2, c = 3; -/
#guard_msgs in
#opencl_check_def int a = 1, b = 2, c = 3;

-- Unsigned types
/-- info: unsigned int x; -/
#guard_msgs in
#opencl_check_def unsigned int x;

/-- info: unsigned char c; -/
#guard_msgs in
#opencl_check_def unsigned char c;

/-- info: unsigned long l; -/
#guard_msgs in
#opencl_check_def unsigned long l;

-- OpenCL scalar types
/-- info: half h; -/
#guard_msgs in
#opencl_check_def half h;

/-- info: uchar uc; -/
#guard_msgs in
#opencl_check_def uchar uc;

/-- info: ushort us; -/
#guard_msgs in
#opencl_check_def ushort us;

/-- info: uint ui; -/
#guard_msgs in
#opencl_check_def uint ui;

/-- info: ulong ul; -/
#guard_msgs in
#opencl_check_def ulong ul;

/-- info: size_t sz; -/
#guard_msgs in
#opencl_check_def size_t sz;

-- Pointer declarations
/-- info: int * p; -/
#guard_msgs in
#opencl_check_def int * p;

/-- info: float * * pp; -/
#guard_msgs in
#opencl_check_def float * * pp;

/-- info: const int * cp; -/
#guard_msgs in
#opencl_check_def const int * cp;

/-- info: int * const pc; -/
#guard_msgs in
#opencl_check_def int * const pc;

-- Address space qualifiers
/-- info: __global int * gp; -/
#guard_msgs in
#opencl_check_def __global int * gp;

/-- info: __local float * lp; -/
#guard_msgs in
#opencl_check_def __local float * lp;

/-- info: __constant int * cp; -/
#guard_msgs in
#opencl_check_def __constant int * cp;

/-- info: __private int x; -/
#guard_msgs in
#opencl_check_def __private int x;

/-- info: global int * gp2; -/
#guard_msgs in
#opencl_check_def global int * gp2;

/-- info: local float * lp2; -/
#guard_msgs in
#opencl_check_def local float * lp2;

-- Array declarations
/-- info: int arr[10]; -/
#guard_msgs in
#opencl_check_def int arr[10];

/-- info: float matrix[4][4]; -/
#guard_msgs in
#opencl_check_def float matrix[4][4];

/-- info: __local int shared[256]; -/
#guard_msgs in
#opencl_check_def __local int shared[256];

-- Vector type declarations
/-- info: float4 pos; -/
#guard_msgs in
#opencl_check_def float4 pos;

/-- info: int2 coord; -/
#guard_msgs in
#opencl_check_def int2 coord;

/-- info: uchar16 data; -/
#guard_msgs in
#opencl_check_def uchar16 data;

/-- info: half4 h4; -/
#guard_msgs in
#opencl_check_def half4 h4;

/-- info: double2 d2; -/
#guard_msgs in
#opencl_check_def double2 d2;

-- Function pointer (basic)
/-- info: int (*fp)(int, float); -/
#guard_msgs in
#opencl_check_def int (*fp)(int, float);

-- Typedef
/-- info: typedef float real; -/
#guard_msgs in
#opencl_check_def typedef float real;

/-- info: typedef __global float * gfloat_ptr; -/
#guard_msgs in
#opencl_check_def typedef __global float * gfloat_ptr;

-- Static and extern
/-- info: static int counter; -/
#guard_msgs in
#opencl_check_def static int counter;

/-- info: extern float externVar; -/
#guard_msgs in
#opencl_check_def extern float externVar;

end DeclTests

--------------------------------------------------------------------------------
/-! ## Expression Tests                                                       -/
--------------------------------------------------------------------------------

section ExprTests

-- Literals
/-- info: 42 -/
#guard_msgs in
#opencl_check_expr 42

/-- info: 0xFF -/
#guard_msgs in
#opencl_check_expr 0xFF

/-- info: 3.14 -/
#guard_msgs in
#opencl_check_expr 3.14

/-- info: 1.0e-5 -/
#guard_msgs in
#opencl_check_expr 1.0e-5

/-- info: 'a' -/
#guard_msgs in
#opencl_check_expr 'a'

/-- info: "hello" -/
#guard_msgs in
#opencl_check_expr "hello"

-- Identifiers
/-- info: x -/
#guard_msgs in
#opencl_check_expr x

/-- info: myVar -/
#guard_msgs in
#opencl_check_expr myVar

-- Arithmetic
/-- info: a + b -/
#guard_msgs in
#opencl_check_expr a + b

/-- info: a - b -/
#guard_msgs in
#opencl_check_expr a - b

/-- info: a * b -/
#guard_msgs in
#opencl_check_expr a * b

/-- info: a / b -/
#guard_msgs in
#opencl_check_expr a / b

/-- info: a % b -/
#guard_msgs in
#opencl_check_expr a % b

/-- info: a + b * c -/
#guard_msgs in
#opencl_check_expr a + b * c

/-- info: (a + b) * c -/
#guard_msgs in
#opencl_check_expr (a + b) * c

-- Comparison
/-- info: a < b -/
#guard_msgs in
#opencl_check_expr a < b

/-- info: a > b -/
#guard_msgs in
#opencl_check_expr a > b

/-- info: a <= b -/
#guard_msgs in
#opencl_check_expr a <= b

/-- info: a >= b -/
#guard_msgs in
#opencl_check_expr a >= b

/-- info: a == b -/
#guard_msgs in
#opencl_check_expr a == b

/-- info: a != b -/
#guard_msgs in
#opencl_check_expr a != b

-- Logical
/-- info: a && b -/
#guard_msgs in
#opencl_check_expr a && b

/-- info: a || b -/
#guard_msgs in
#opencl_check_expr a || b

/-- info: !a -/
#guard_msgs in
#opencl_check_expr !a

-- Bitwise
/-- info: a & b -/
#guard_msgs in
#opencl_check_expr a & b

/-- info: a | b -/
#guard_msgs in
#opencl_check_expr a | b

/-- info: a ^ b -/
#guard_msgs in
#opencl_check_expr a ^ b

-- /-- info: ~a -/
-- #guard_msgs in
-- #opencl_check_expr ~a

/-- info: a << 2 -/
#guard_msgs in
#opencl_check_expr a << 2

/-- info: a >> 2 -/
#guard_msgs in
#opencl_check_expr a >> 2

-- Unary
/-- info: -x -/
#guard_msgs in
#opencl_check_expr -x

/-- info: +x -/
#guard_msgs in
#opencl_check_expr +x

/-- info: &x -/
#guard_msgs in
#opencl_check_expr &x

/-- info: *p -/
#guard_msgs in
#opencl_check_expr *p

/-- info: ++i -/
#guard_msgs in
#opencl_check_expr ++i

-- /-- info: --i -/
-- #guard_msgs in
-- #opencl_check_expr --i

/-- info: i++ -/
#guard_msgs in
#opencl_check_expr i++

-- /-- info: i-- -/
-- #guard_msgs in
-- #opencl_check_expr i--

-- Ternary
/-- info: a ? b : c -/
#guard_msgs in
#opencl_check_expr a ? b : c

/-- info: x > 0 ? x : -x -/
#guard_msgs in
#opencl_check_expr x > 0 ? x : -x

-- Assignment
/-- info: x = 5 -/
#guard_msgs in
#opencl_check_expr x = 5

/-- info: x += 1 -/
#guard_msgs in
#opencl_check_expr x += 1

/-- info: x -= 1 -/
#guard_msgs in
#opencl_check_expr x -= 1

/-- info: x *= 2 -/
#guard_msgs in
#opencl_check_expr x *= 2

/-- info: x /= 2 -/
#guard_msgs in
#opencl_check_expr x /= 2

/-- info: x &= mask -/
#guard_msgs in
#opencl_check_expr x &= mask

/-- info: x |= flag -/
#guard_msgs in
#opencl_check_expr x |= flag

/-- info: x <<= 2 -/
#guard_msgs in
#opencl_check_expr x <<= 2

/-- info: x >>= 2 -/
#guard_msgs in
#opencl_check_expr x >>= 2

-- Array subscript
/-- info: arr[0] -/
#guard_msgs in
#opencl_check_expr arr[0]

/-- info: arr[i] -/
#guard_msgs in
#opencl_check_expr arr[i]

/-- info: matrix[i][j] -/
#guard_msgs in
#opencl_check_expr matrix[i][j]

-- Function call
/-- info: foo() -/
#guard_msgs in
#opencl_check_expr foo()

/-- info: bar(x) -/
#guard_msgs in
#opencl_check_expr bar(x)

/-- info: baz(a, b, c) -/
#guard_msgs in
#opencl_check_expr baz(a, b, c)

/-- info: get_global_id(0) -/
#guard_msgs in
#opencl_check_expr get_global_id(0)

-- Member access
/-- info: s.x -/
#guard_msgs in
#opencl_check_expr s.x

/-- info: p->x -/
#guard_msgs in
#opencl_check_expr p->x

-- Vector swizzle
/-- info: v.x -/
#guard_msgs in
#opencl_check_expr v.x

/-- info: v.xy -/
#guard_msgs in
#opencl_check_expr v.xy

/-- info: v.xyz -/
#guard_msgs in
#opencl_check_expr v.xyz

/-- info: v.xyzw -/
#guard_msgs in
#opencl_check_expr v.xyzw

/-- info: v.rgba -/
#guard_msgs in
#opencl_check_expr v.rgba

/-- info: v.s0 -/
#guard_msgs in
#opencl_check_expr v.s0

/-- info: v.s01 -/
#guard_msgs in
#opencl_check_expr v.s01

/-- info: v.s0123 -/
#guard_msgs in
#opencl_check_expr v.s0123

/-- info: v.hi -/
#guard_msgs in
#opencl_check_expr v.hi

/-- info: v.lo -/
#guard_msgs in
#opencl_check_expr v.lo

/-- info: v.even -/
#guard_msgs in
#opencl_check_expr v.even

/-- info: v.odd -/
#guard_msgs in
#opencl_check_expr v.odd

-- Vector literal (compound literal syntax)
/-- info: (float4){1.0, 2.0, 3.0, 4.0} -/
#guard_msgs in
#opencl_check_expr (float4){1.0, 2.0, 3.0, 4.0}

/-- info: (int2){0, 1} -/
#guard_msgs in
#opencl_check_expr (int2){0, 1}

/-- info: (uchar4){255, 128, 64, 0} -/
#guard_msgs in
#opencl_check_expr (uchar4){255, 128, 64, 0}

-- Cast
/-- info: (float)x -/
#guard_msgs in
#opencl_check_expr (float)x

/-- info: (int)f -/
#guard_msgs in
#opencl_check_expr (int)f

/-- info: (__global float *)p -/
#guard_msgs in
#opencl_check_expr (__global float *)p

-- Sizeof
/-- info: sizeof(int) -/
#guard_msgs in
#opencl_check_expr sizeof(int)

/-- info: sizeof(float4) -/
#guard_msgs in
#opencl_check_expr sizeof(float4)

/-- info: sizeof(x) -/
#guard_msgs in
#opencl_check_expr sizeof(x)

-- Complex expressions
/-- info: a + b * c - d / e -/
#guard_msgs in
#opencl_check_expr a + b * c - d / e

/-- info: (a + b) * (c - d) -/
#guard_msgs in
#opencl_check_expr (a + b) * (c - d)

/-- info: arr[i] + arr[j] -/
#guard_msgs in
#opencl_check_expr arr[i] + arr[j]

/-- info: foo(bar(x)) -/
#guard_msgs in
#opencl_check_expr foo(bar(x))

/-- info: p->data[i].value -/
#guard_msgs in
#opencl_check_expr p->data[i].value


end ExprTests

--------------------------------------------------------------------------------
/-! ## Type Tests                                                             -/
--------------------------------------------------------------------------------

section TypeTests

/-- info: int -/
#guard_msgs in
#opencl_check_type int

/-- info: float -/
#guard_msgs in
#opencl_check_type float

/-- info: void -/
#guard_msgs in
#opencl_check_type void

/-- info: unsigned int -/
#guard_msgs in
#opencl_check_type unsigned int

/-- info: signed char -/
#guard_msgs in
#opencl_check_type signed char

/-- info: long long -/
#guard_msgs in
#opencl_check_type long long

-- OpenCL types
/-- info: half -/
#guard_msgs in
#opencl_check_type half

/-- info: uchar -/
#guard_msgs in
#opencl_check_type uchar

/-- info: uint -/
#guard_msgs in
#opencl_check_type uint

/-- info: size_t -/
#guard_msgs in
#opencl_check_type size_t

-- Vector types
/-- info: float4 -/
#guard_msgs in
#opencl_check_type float4

/-- info: int2 -/
#guard_msgs in
#opencl_check_type int2

/-- info: uchar16 -/
#guard_msgs in
#opencl_check_type uchar16

/-- info: double8 -/
#guard_msgs in
#opencl_check_type double8

-- Pointers
/-- info: int * -/
#guard_msgs in
#opencl_check_type int *

/-- info: float * * -/
#guard_msgs in
#opencl_check_type float * *

/-- info: const int * -/
#guard_msgs in
#opencl_check_type const int *

/-- info: int * const -/
#guard_msgs in
#opencl_check_type int * const

-- Address spaces
/-- info: __global int * -/
#guard_msgs in
#opencl_check_type __global int *

/-- info: __local float * -/
#guard_msgs in
#opencl_check_type __local float *

/-- info: __constant int * -/
#guard_msgs in
#opencl_check_type __constant int *

/-- info: __private float -/
#guard_msgs in
#opencl_check_type __private float

-- Image types
/-- info: image2d_t -/
#guard_msgs in
#opencl_check_type image2d_t

/-- info: image3d_t -/
#guard_msgs in
#opencl_check_type image3d_t

/-- info: sampler_t -/
#guard_msgs in
#opencl_check_type sampler_t

-- With access qualifiers
/-- info: __read_only image2d_t -/
#guard_msgs in
#opencl_check_type __read_only image2d_t

/-- info: __write_only image2d_t -/
#guard_msgs in
#opencl_check_type __write_only image2d_t

/-- info: read_only image3d_t -/
#guard_msgs in
#opencl_check_type read_only image3d_t

end TypeTests

--------------------------------------------------------------------------------
/-! ## Statement Tests                                                        -/
--------------------------------------------------------------------------------

section StmtTests

-- Expression statements
/-- info: { x = 5; } -/
#guard_msgs in
#opencl_check { x = 5; }

/-- info: { foo(); } -/
#guard_msgs in
#opencl_check { foo(); }

/-- info: { i++; } -/
#guard_msgs in
#opencl_check { i++; }

-- Declarations in statement context
/-- info: { int x = 0; } -/
#guard_msgs in
#opencl_check { int x = 0; }

/-- info: { float4 pos = (float4){0.0, 0.0, 0.0, 1.0}; } -/
#guard_msgs in
#opencl_check { float4 pos = (float4){0.0, 0.0, 0.0, 1.0}; }

-- If statements
/-- info: { if (x > 0) y = x; } -/
#guard_msgs in
#opencl_check { if (x > 0) y = x; }

/-- info: { if (x > 0) { y = x; } } -/
#guard_msgs in
#opencl_check { if (x > 0) { y = x; } }

/-- info: { if (x > 0) { y = x; } else { y = -x; } } -/
#guard_msgs in
#opencl_check { if (x > 0) { y = x; } else { y = -x; } }

/--
info: {
  if (x > 0) {
    y = x;
  } else if (x < 0) {
    y = -x;
  } else {
    y = 0;
  }
}
-/
#guard_msgs in
#opencl_check {
  if (x > 0) {
    y = x;
  } else if (x < 0) {
    y = -x;
  } else {
    y = 0;
  }
}

-- While loops
/-- info: { while (i < n) i++; } -/
#guard_msgs in
#opencl_check { while (i < n) i++; }

/--
info: {
  while (i < n) {
    sum += arr[i];
    i++;
  }
}
-/
#guard_msgs in
#opencl_check {
  while (i < n) {
    sum += arr[i];
    i++;
  }
}

-- Do-while loops
/-- info: { do { i++; } while (i < n) } -/
#guard_msgs in
#opencl_check { do { i++; } while (i < n) }

-- For loops
/-- info: { for (int i = 0; i < n; i++) sum += arr[i]; } -/
#guard_msgs in
#opencl_check { for (int i = 0; i < n; i++) sum += arr[i]; }

/--
info: {
  for (int i = 0; i < n; i++) {
    sum += arr[i];
  }
}
-/
#guard_msgs in
#opencl_check {
  for (int i = 0; i < n; i++) {
    sum += arr[i];
  }
}

/-- info: { for (;;) { break; } } -/
#guard_msgs in
#opencl_check { for (;;) { break; } }

-- Switch statements
/--
info: {
  switch (x) {
    case 0: y = 0; break;
    case 1: y = 1; break;
    default: y = -1; break;
  }
}
-/
#guard_msgs in
#opencl_check {
  switch (x) {
    case 0: y = 0; break;
    case 1: y = 1; break;
    default: y = -1; break;
  }
}

-- Jump statements
/-- info: { return; } -/
#guard_msgs in
#opencl_check { return; }

/--
info: {
  // return zero
  return 0;
}
-/
#guard_msgs in
#opencl_check {
  // return zero
  return 0;
}

/-- info: { return x + y; } -/
#guard_msgs in
#opencl_check { return x + y; }

/-- info: { break; } -/
#guard_msgs in
#opencl_check { break; }

/-- info: { continue; } -/
#guard_msgs in
#opencl_check { continue; }

-- Compound statements
/--
info: {
  {
    int x = 0;
    int y = 1;
    int z = x + y;
  }
}
-/
#guard_msgs in
#opencl_check {
  {
    int x = 0;
    int y = 1;
    int z = x + y;
  }
}

-- Multiple statements
/--
info: {
  int gid = get_global_id(0);
  if (gid < n) {
    output[gid] = input[gid] * 2.0;
  }
}
-/
#guard_msgs in
#opencl_check {
  int gid = get_global_id(0);
  if (gid < n) {
    output[gid] = input[gid] * 2.0;
  }
}

end StmtTests

--------------------------------------------------------------------------------
/-! ## OpenCL Kernel Pattern Tests                                            -/
--------------------------------------------------------------------------------

section KernelPatternTests

-- Typical kernel variable setup
/--
info: {
  int gid = get_global_id(0);
  int lid = get_local_id(0);
  int gsize = get_global_size(0);
  int lsize = get_local_size(0);
}
-/
#guard_msgs in
#opencl_check {
  int gid = get_global_id(0);
  int lid = get_local_id(0);
  int gsize = get_global_size(0);
  int lsize = get_local_size(0);
}

-- Vector operations
/--
info: {
  float4 a = vload4(0, input);
  float4 b = vload4(0, input + 4);
  float4 c = a + b;
  vstore4(c, 0, output);
}
-/
#guard_msgs in
#opencl_check {
  float4 a = vload4(0, input);
  float4 b = vload4(0, input + 4);
  float4 c = a + b;
  vstore4(c, 0, output);
}

-- Barrier usage
/-- info: { barrier(CLK_LOCAL_MEM_FENCE); } -/
#guard_msgs in
#opencl_check { barrier(CLK_LOCAL_MEM_FENCE); }

/-- info: { barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE); } -/
#guard_msgs in
#opencl_check { barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE); }

-- Local memory pattern
/--
info: {
  __local float scratch[256];
  int lid = get_local_id(0);
  scratch[lid] = input[get_global_id(0)];
  barrier(CLK_LOCAL_MEM_FENCE);
}
-/
#guard_msgs in
#opencl_check {
  __local float scratch[256];
  int lid = get_local_id(0);
  scratch[lid] = input[get_global_id(0)];
  barrier(CLK_LOCAL_MEM_FENCE);
}

-- Image sampling
/-- info: { float4 pixel = read_imagef(img, sampler, coord); } -/
#guard_msgs in
#opencl_check { float4 pixel = read_imagef(img, sampler, coord); }

/-- info: { write_imagef(output, coord, pixel); } -/
#guard_msgs in
#opencl_check { write_imagef(output, coord, pixel); }

-- Atomic operations
/-- info: { atomic_add(counter, 1); } -/
#guard_msgs in
#opencl_check { atomic_add(counter, 1); }

/-- info: { int old = atomic_cmpxchg(ptr, expected, desired); } -/
#guard_msgs in
#opencl_check { int old = atomic_cmpxchg(ptr, expected, desired); }

-- Math functions
/--
info: {
  float s = sin(x);
  float c = cos(x);
  float len = length(v);
  float3 n = normalize(v);
  float d = dot(a, b);
  float3 cr = cross(a, b);
}
-/
#guard_msgs in
#opencl_check {
  float s = sin(x);
  float c = cos(x);
  float len = length(v);
  float3 n = normalize(v);
  float d = dot(a, b);
  float3 cr = cross(a, b);
}

-- Type conversions
/--
info: {
  int i = convert_int(f);
  float f = convert_float(i);
  int4 iv = convert_int4(fv);
}
-/
#guard_msgs in
#opencl_check {
  int i = convert_int(f);
  float f = convert_float(i);
  int4 iv = convert_int4(fv);
}

end KernelPatternTests

--------------------------------------------------------------------------------
/-! ## Struct and Enum Tests                                                  -/
--------------------------------------------------------------------------------

section StructEnumTests

/-- info: struct Point { float x; float y; }; -/
#guard_msgs in
#opencl_check_def struct Point { float x; float y; };

/--
info: struct Particle {
  float4 position;
  float4 velocity;
  float mass;
};
-/
#guard_msgs in
#opencl_check_def struct Particle {
  float4 position;
  float4 velocity;
  float mass;
};

/-- info: typedef struct { float x; float y; float z; } Vec3; -/
#guard_msgs in
#opencl_check_def typedef struct { float x; float y; float z; } Vec3;

/-- info: union Data { int i; float f; }; -/
#guard_msgs in
#opencl_check_def union Data { int i; float f; };

/-- info: enum Color { RED, GREEN, BLUE }; -/
#guard_msgs in
#opencl_check_def enum Color { RED, GREEN, BLUE };

/-- info: enum Status { OK = 0, ERROR = 1, PENDING = 2 }; -/
#guard_msgs in
#opencl_check_def enum Status { OK = 0, ERROR = 1, PENDING = 2 };

end StructEnumTests

end HouLean.OpenCL
