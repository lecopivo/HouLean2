/-! Additional operations on Arrays that are common in Houdini -/
def Array.add {α} [Add α] [Zero α] (xs ys : Array α) : Array α := 
  -- extend array to the same size
  let n := max xs.size ys.size
  let xs := xs ++ (Array.replicate (n - xs.size) 0)
  let ys := ys ++ (Array.replicate (n - ys.size) 0)
  -- add values
  (xs.zip ys).map (fun (x,y) => x + y)

def Array.sub {α} [Sub α] [Zero α] (xs ys : Array α) : Array α := 
  -- extend array to the same size
  let n := max xs.size ys.size
  let xs := xs ++ (Array.replicate (n - xs.size) 0)
  let ys := ys ++ (Array.replicate (n - ys.size) 0)
  -- sub values
  (xs.zip ys).map (fun (x,y) => x - y)

def Array.mul {α} [Mul α] [One α] (xs ys : Array α) : Array α := 
  -- extend array to the same size
  let n := max xs.size ys.size
  let xs := xs ++ (Array.replicate (n - xs.size) 1)
  let ys := ys ++ (Array.replicate (n - ys.size) 1)
  -- mul values
  (xs.zip ys).map (fun (x,y) => x * y)

def Array.div {α} [Div α] [One α] (xs ys : Array α) : Array α := 
  -- extend array to the same size
  let n := max xs.size ys.size
  let xs := xs ++ (Array.replicate (n - xs.size) 1)
  let ys := ys ++ (Array.replicate (n - ys.size) 1)
  -- div values
  (xs.zip ys).map (fun (x,y) => x / y)

def Array.scalarMul {α β} [HMul α β β] (x : α) (ys : Array β) : Array β := 
  ys.map (fun y => x * y)

def Array.scalarMul' {α β} [HMul α β α] (xs : Array α) (y : β) : Array α := 
  xs.map (fun x => x * y)

def Array.smul {α β} [SMul α β] (x : α) (ys : Array β) : Array β := 
  ys.map (fun y => x • y)

namespace HouLean
scoped instance {α} [Add α] [Zero α] : Add (Array α) := ⟨Array.add⟩
scoped instance {α} [Sub α] [Zero α] : Sub (Array α) := ⟨Array.sub⟩
scoped instance {α} [Mul α] [One α] : Mul (Array α) := ⟨Array.mul⟩
scoped instance {α} [Div α] [One α] : Div (Array α) := ⟨Array.div⟩
scoped instance {α β} [HMul α β β] : HMul α (Array β) (Array β) := ⟨Array.scalarMul⟩
scoped instance {α β} [HMul α β α] : HMul (Array α) β (Array α) := ⟨Array.scalarMul'⟩
scoped instance {α β} [SMul α β] : SMul α (Array β) := ⟨Array.smul⟩
end HouLean
