import HouLean.OpenCL.Compiler.SpecAndSimp
import HouLean.OpenCL.Compiler.Main


open Lean Meta

namespace HouLean.OpenCL.Compiler

open Meta

open SpecializeAndSimp in
def fullCompile (e : Expr) : MetaM MessageData := do
  withoutModifyingEnv do

    forallTelescope (← inferType e) fun xs r => do
      let body := e.beta xs

      let (body', s) ← (specializeAndSimp body).runInMeta
        {} { zeta := false } #[`opencl_csimp] specializeImplementedBy

      let go := do
        withFVars xs fun varIds => do
        compileBlock body'

        let rt' ← compileType r
        let ts' ← xs.mapM (fun x => inferType x >>= compileType)
        let returnType : TSyntax `clDeclSpec ← `(clDeclSpec| $rt':ident)
        let argTypes ← ts'.mapM fun t => `(clTypeSpec| $t:ident)
        let stmts := (← get).statements

        let mainId := mkIdent (.mkSimple "main")
        `(clFunction| $returnType $mainId:ident($[$argTypes:clTypeSpec $varIds:ident],*) { $stmts* })

      let funs ← s.specOrder.mapM compileDecl
      let (main, _) ← go {} {}

      let mut msg : MessageData := m!""

      for f in funs do
        msg := msg ++ m!"{f}\n\n"
      msg := msg ++ m!"{main}"

      return msg


open Lean Elab Command Meta in
elab c:"#opencl_compile " e:term : command => do
  liftTermElabM do
    let e ← Term.elabTermAndSynthesize e none
    let m ← fullCompile e
    logInfoAt c m

#exit

#opencl_sas (fun x y : Vector Float32 3 => x.dot y)
#opencl_compile (fun x y : Vector Float32 3 => x[0])
#opencl_compile (fun x y : Matrix Float32 3 3 => x * y)
#opencl_compile (fun x y : Float32 => x + y)
#opencl_compile (fun x y : Matrix Float32 3 3 => x + y)

#opencl_compile (fun (A : Matrix Float32 4 4) (x : Vector Float32 3) => x.split1)

#opencl_compile (fun (A : Matrix Float32 4 4) (x : Vector Float32 3) =>
  let a := Matrix.ofFn (m:=3) (n:=3) fun i j _ => A[i,j]
  let b := Matrix.ofFn (m:=3) (n:=3) fun i j _ => A[i+1,j+1]
  let c := (a, b)
  c)


/-- error: Don't know how to compile type: (i j : Nat) → i < 3 ∧ j < 3 → Float32 -/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 4 4) => A.split1)

attribute [opencl_csimp] Matrix.ofFn

/--
info: Prod_Prod_matrix33float_float3_Prod_float3_float houlean_matrix_split1_3_float32(matrix44float a){
      return
        (Prod_Prod_matrix33float_float3_Prod_float3_float){(Prod_matrix33float_float3){(matrix33float){(float3){a.row0.x,
                      a.row0.y, a.row0.z},
                  (float3){a.row1.x, a.row1.y, a.row1.z}, (float3){a.row2.x, a.row2.y, a.row2.z}},
              (float3){a.row0.w, a.row1.w, a.row2.w}},
          (Prod_float3_float){(float3){a.row3.x, a.row3.y, a.row3.z}, a.row3.w}};
}

Prod_Prod_matrix33float_float3_Prod_float3_float main(matrix44float A){
      return houlean_matrix_split1_3_float32(A);
}
-/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 4 4) => A.split1)


set_option trace.HouLean.OpenCL.compiler true in
#opencl_compile (fun (x : Float32 × Float32) =>
  let x := x
  x.1)

#opencl_compile (fun (x : Float32 × Float32) =>
 let x := x
 x.1)

#opencl_compile (fun x : Float32 =>
  let a := (x,x,x)
  a)

/--
info: Prod_Prod_matrix33float_float3_Prod_float3_float houlean_matrix_split1_3_float32(matrix44float a){
      return
        (Prod_Prod_matrix33float_float3_Prod_float3_float){(Prod_matrix33float_float3){(matrix33float){(float3){a.row0.x,
                      a.row0.y, a.row0.z},
                  (float3){a.row1.x, a.row1.y, a.row1.z}, (float3){a.row2.x, a.row2.y, a.row2.z}},
              (float3){a.row0.w, a.row1.w, a.row2.w}},
          (Prod_float3_float){(float3){a.row3.x, a.row3.y, a.row3.z}, a.row3.w}};
}

float vector_dot_float32_3(float3 u, float3 v){
      const float a = u.x * v.x;
      const float a1 = a + u.y * v.y;
      const float a2 = a1 + u.z * v.z;
      return a2;
}

float3 houlean_matrix_mulvec_float32_3_3(matrix33float a, float3 v){
      return
        (float3){vector_dot_float32_3(a.row0, v), vector_dot_float32_3(a.row1, v), vector_dot_float32_3(a.row2, v)};
}

float3 hmul_hmul_matrix_float32_3_3_vector_float32_3_vector_float32_3(matrix33float a, float3 a1){
      return houlean_matrix_mulvec_float32_3_3(a, a1);
}

float inv_inv_float32(float a){
      return 1.0 / a;
}

float3 hdiv_hdiv_vector_float32_3_float32_vector_float32_3(float3 a, float a1){
      const float is = inv_inv_float32(a1);
      return is * a;
}

float3 houlean_matrix_transformpointright_float32_3(matrix44float transform, float3 point){
      const Prod_Prod_matrix33float_float3_Prod_float3_float tmp = houlean_matrix_split1_3_float32(transform);
      const float w = vector_dot_float32_3(point, tmp.snd.fst) + tmp.snd.snd;
      return
        hdiv_hdiv_vector_float32_3_float32_vector_float32_3(hmul_hmul_matrix_float32_3_3_vector_float32_3_vector_float32_3(tmp.fst.fst,
              point) +
            tmp.fst.snd,
          w);
}

float3 main(matrix44float A, float3 x){
      return houlean_matrix_transformpointright_float32_3(A, x);
}
-/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 4 4) (x : Vector Float32 3) => A.transformPointRight x)


#opencl_compile (fun (t : RigidTransform) (x : Vector3) =>
  let t := t
  t.transformPoint x)


def foo := (fun x y : Matrix Float32 3 3 => x + y)
#opencl_compile (fun x y => foo x y)


/--
info: double main(double x){
      return acos(x);
}
-/
#guard_msgs in
#opencl_compile (fun x : Float => Math.acos x)





/--
info: float vector_length2_float32_3(float3 u){
      const float a = u.x * u.x;
      const float a1 = a + u.y * u.y;
      const float a2 = a1 + u.z * u.z;
      return a2;
}

float vector_length_float32_3(float3 u){
      return sqrt(vector_length2_float32_3(u));
}

bool houlean_math_approxequal_approxequal_float32_float32_0e0_1e_9(float x){
      return 1e-10 >= fabs(x - 0.0);
}

float inv_inv_float32(float a){
      return 1.0 / a;
}

float3 hdiv_hdiv_vector_float32_3_float32_vector_float32_3(float3 a, float a1){
      const float is = inv_inv_float32(a1);
      return is * a;
}

Prod_float3_float vector_normalize_float32_3(float3 u){
      const float len = vector_length_float32_3(u);
      if (a == b)
        {
              return (Prod_float3_float){u, 0.0};
        } else
        {
              return (Prod_float3_float){hdiv_hdiv_vector_float32_3_float32_vector_float32_3(u, len), len};
        }
}

float3 vector_normalized_float32_3(float3 u){
      return vector_normalize_float32_3(u).fst;
}

float vector_dot_float32_3(float3 u, float3 v){
      const float a = u.x * v.x;
      const float a1 = a + u.y * v.y;
      const float a2 = a1 + u.z * v.z;
      return a2;
}

float3 vector_slerp_float32_3(float3 v, float3 w, float t){
      const float d = vector_dot_float32_3(vector_normalized_float32_3(v), vector_normalized_float32_3(w));
      const float d1 = clamp(d, -1.0, 1.0);
      const float theta = acos(d1);
      const float s = sin(theta);
      const float a = sin(1.0 - t * theta) / s;
      const float b = sin(t * theta) / s;
      if (a == b)
        {
              return mix(v, w, t);
        } else
        {
              return a * v + b * w;
        }
}

float3 houlean_math_slerp_slerp_vector_float32_3_float32(float3 x, float3 y, float t){
      return vector_slerp_float32_3(x, y, t);
}

float3 main(float3 x, float3 y, float w){
      return houlean_math_slerp_slerp_vector_float32_3_float32(x, y, w);
}
-/
#guard_msgs in
#opencl_compile (fun (x y : Vector Float32 3) (w : Float32) => Math.slerp x y w)
