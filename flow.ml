open Graph
open Graph.Flow

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

module BaseGraph = Imperative.Digraph.ConcreteLabeled (Coord) (Int)

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

(* let a = Test.create ()

   let c00 : HexUtil.coord = { col = 0; diag = 0 }

   let c10 : HexUtil.coord = { col = 1; diag = 0 }

   let b = Test.add_vertex a c00; Test.add_vertex a c10; Test.add_edge a
   c00 c10 *)
