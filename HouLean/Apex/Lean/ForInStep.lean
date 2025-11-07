import HouLean.Init
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler


/-- Currently we do not support loop termination. Therfore we compile `ForInStep α` directly to `A`.

In future, we might change it to `A × Bool` 
-/
instance {α: Type } [ApexType α A] : ApexType (ForInStep α) A where
  toApex x := toApex x.value
  fromApex x := .yield (fromApex x)

run_meta compilerExt.add (.implementedByName ``ForInStep.yield ``id' #[some 0, some 1]) default
run_meta compilerExt.add (.implementedByName ``ForInStep.value ``toApex #[none,none,none,some 1]) default
-- Loop termination is not supported so do not implement this!
-- run_meta compilerExt.add (.implementedByName ``ForInStep.done ``id' #[some 0, some 1]) default
