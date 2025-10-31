import HouLean.Init
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

run_meta compilerExt.add (.implementedByName ``Decidable ``Bool #[]) default
run_meta compilerExt.add (.implementedByName ``decide ``id' #[none, some 1]) default

