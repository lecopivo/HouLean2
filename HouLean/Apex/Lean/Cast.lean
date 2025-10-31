import HouLean.Init
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

run_meta compilerExt.add (.implementedByName ``unsafeCast ``id' #[some 0, some 2]) default
run_meta compilerExt.add (.implementedByName ``cast ``id' #[some 0, some 3]) default
