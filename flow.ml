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
