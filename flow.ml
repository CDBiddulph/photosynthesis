open Graph
open Graph.Flow
open Graph.Sig

module Coord = struct
  open HexUtil

  type t = coord

  let hash c = (10 * c.col) + c.diag

  let compare x y = compare (hash x) (hash y)

  let equal = ( = )

  let default = { col = 0; diag = 0 }
end

module Int = struct
  type t = int

  let compare = Stdlib.compare

  let hash = Hashtbl.hash

  let equal = ( = )

  let default = 0
end

module BaseGraph = Persistent.Digraph.ConcreteLabeled (Coord) (Int)

module FlowMin = struct
  type t = Int.t

  type label = Int.t

  let max_capacity l = 1

  let flow x = x

  let add = ( + )

  let sub = ( - )

  let zero = Int.default

  let compare = compare

  let min_capacity l = 1
end

module MaxFlow = Ford_Fulkerson (BaseGraph) (FlowMin)

(* let c00 : HexUtil.coord = { col = 0; diag = 0 }

   let c10 : HexUtil.coord = { col = 1; diag = 0 }

   let c01 : HexUtil.coord = { col = 0; diag = 1 }

   let c11 : HexUtil.coord = { col = 1; diag = 1 }

   let a = BaseGraph.add_vertex (BaseGraph.add_vertex BaseGraph.empty
   c00) c11

   let b = BaseGraph.add_vertex (BaseGraph.add_vertex a c01) c10

   let c = BaseGraph.add_edge (BaseGraph.add_edge b c00 c01) c00 c10

   let d = BaseGraph.add_edge (BaseGraph.add_edge c c10 c11) c01 c11 *)
