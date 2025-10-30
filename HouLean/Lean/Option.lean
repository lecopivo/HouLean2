import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex

@[apex_implements Option.some]
def Option.some.apex_impl {α} (x : α) : α × Bool := (x, true)

@[apex_implements Option.none]
def Option.none.apex_impl {α} [Inhabited α] : α × Bool := (default, false)
