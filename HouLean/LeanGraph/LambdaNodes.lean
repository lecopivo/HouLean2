import HouLean.LeanGraph.Init

namespace HouLean

@[lean_graph_node "input"]
def input (type : Type) [Inhabited type] : type := default


set_option linter.unusedVariables false in
@[lean_graph_node "output"]
def output {type : Type} (output : type) : Unit := ()


set_option linter.unusedVariables false in
@[lean_graph_node "eval"]
def eval {type : Type} (x : type) : type := x


