import HouLean.Init
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

run_meta compilerExt.add (.implementedByName ``ForInStep ``Id' #[some 0]) default
run_meta compilerExt.add (.implementedByName ``ForInStep.yield ``id' #[some 0, some 1]) default
