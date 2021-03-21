(** A 2-dimensional [Cell.t array] to represent a hexagonal board with
    minimum width 4 and height 7. The "horizontal" diagonal is within
    the same 1-dimensional array, with direction 0 being in increasing
    index (i.e. map.(0).(1) is in direction 0 of map.(0).(0)). Lower
    indices (for purposes of representation and easier understanding)
    indicate closeness to the "top" and "left" side of of the board.
    "Columns" of [HexUtil.coord] are the first index in the
    [Cell.t array array], and the diagonals are the second.*)
type t = Cell.t option array array

let init_map : t =
  let open Cell in
  [|
    [|
      Some (init_cell 1 None);
      Some (init_cell 1 None);
      Some (init_cell 1 None);
      Some (init_cell 1 None);
    |];
    [|
      Some (init_cell 1 None);
      Some (init_cell 2 None);
      Some (init_cell 2 None);
      Some (init_cell 2 None);
      Some (init_cell 1 None);
    |];
    [|
      Some (init_cell 1 None);
      Some (init_cell 2 None);
      Some (init_cell 3 None);
      Some (init_cell 3 None);
      Some (init_cell 2 None);
      Some (init_cell 1 None);
    |];
    [|
      Some (init_cell 1 None);
      Some (init_cell 2 None);
      Some (init_cell 3 None);
      Some (init_cell 4 None);
      Some (init_cell 3 None);
      Some (init_cell 2 None);
      Some (init_cell 1 None);
    |];
    [|
      Some (init_cell 1 None);
      Some (init_cell 2 None);
      Some (init_cell 3 None);
      Some (init_cell 3 None);
      Some (init_cell 2 None);
      Some (init_cell 1 None);
    |];
    [|
      Some (init_cell 1 None);
      Some (init_cell 2 None);
      Some (init_cell 2 None);
      Some (init_cell 2 None);
      Some (init_cell 1 None);
    |];
    [|
      Some (init_cell 1 None);
      Some (init_cell 1 None);
      Some (init_cell 1 None);
      Some (init_cell 1 None);
    |];
  |]

let cell_at (map : t) coord : Cell.t option =
  let open HexUtil in
  map.(coord.col).(coord.diag)

let set_cell (map : t) cell coord : t =
  let open HexUtil in
  map.(coord.col).(coord.diag) <- cell;
  map

let does_block (map : t) (d : HexUtil.dir) c1 c2 =
  let open HexUtil in
  print_endline "here";
  match d with
  | 0 -> c1.col = c2.col && c1.diag = c2.diag - 1
  | 1 -> c1.col + 1 = c2.col && c1.diag = c2.diag - 1
  | 2 -> c1.col + 1 = c2.col && c1.diag = c2.diag
  | 3 -> c1.col = c2.col && c1.diag = c2.diag - 1
  | 4 -> c1.col - 1 = c2.col && c1.diag = c2.diag - 1
  | 5 -> c1.col - 1 = c2.col && c1.diag = c2.diag
  | _ -> failwith "Invalid direction"

let dist (map : t) c1 c2 =
  let open HexUtil in
  Int.abs (c1.col - c2.col) + Int.abs (c1.diag - c2.diag)

let valid_coord (map : t) c =
  let open HexUtil in
  c.col >= 0 && c.diag >= 0
  && c.col < Array.length map
  && c.diag < Array.length map.(c.col)

let neighbor (map : t) c (d : HexUtil.dir) =
  let open HexUtil in
  let new_coord =
    match d with
    | 0 -> { c with diag = c.diag + 1 }
    | 1 -> { col = c.col + 1; diag = c.diag + 1 }
    | 2 -> { c with col = c.col + 1 }
    | 3 -> { c with diag = c.diag - 1 }
    | 4 -> { col = c.col - 1; diag = c.diag + 1 }
    | 5 -> { c with col = c.col - 1 }
    | _ -> failwith "Invalid direction"
  in
  if valid_coord map new_coord then Some new_coord else None
