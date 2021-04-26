(** A 2-dimensional [Cell.t array] to represent a hexagonal board with
    minimum width 4 and height 7. Lower indices (for purposes of
    representation and easier understanding) indicate closeness to the
    "top" and "left" side of of the board. "Columns" of [HexUtil.coord]
    are the first index in the [Cell.t array array], and the diagonals
    are the second.*)
type t = Cell.t option array array

let init_map () : t =
  let open Cell in
  [|
    [|
      Some (init_cell 1 None { col = 0; diag = 0 });
      Some (init_cell 1 None { col = 0; diag = 1 });
      Some (init_cell 1 None { col = 0; diag = 2 });
      Some (init_cell 1 None { col = 0; diag = 3 });
      None;
      None;
      None;
    |];
    [|
      Some (init_cell 1 None { col = 1; diag = 0 });
      Some (init_cell 2 None { col = 1; diag = 1 });
      Some (init_cell 2 None { col = 1; diag = 2 });
      Some (init_cell 2 None { col = 1; diag = 3 });
      Some (init_cell 1 None { col = 1; diag = 4 });
      None;
      None;
    |];
    [|
      Some (init_cell 1 None { col = 2; diag = 0 });
      Some (init_cell 2 None { col = 2; diag = 1 });
      Some (init_cell 3 None { col = 2; diag = 2 });
      Some (init_cell 3 None { col = 2; diag = 3 });
      Some (init_cell 2 None { col = 2; diag = 4 });
      Some (init_cell 1 None { col = 2; diag = 5 });
      None;
    |];
    [|
      Some (init_cell 1 None { col = 3; diag = 0 });
      Some (init_cell 2 None { col = 3; diag = 1 });
      Some (init_cell 3 None { col = 3; diag = 2 });
      Some (init_cell 4 None { col = 3; diag = 3 });
      Some (init_cell 3 None { col = 3; diag = 4 });
      Some (init_cell 2 None { col = 3; diag = 5 });
      Some (init_cell 1 None { col = 3; diag = 6 });
    |];
    [|
      None;
      Some (init_cell 1 None { col = 4; diag = 1 });
      Some (init_cell 2 None { col = 4; diag = 2 });
      Some (init_cell 3 None { col = 4; diag = 3 });
      Some (init_cell 3 None { col = 4; diag = 4 });
      Some (init_cell 2 None { col = 4; diag = 5 });
      Some (init_cell 1 None { col = 4; diag = 6 });
    |];
    [|
      None;
      None;
      Some (init_cell 1 None { col = 5; diag = 2 });
      Some (init_cell 2 None { col = 5; diag = 3 });
      Some (init_cell 2 None { col = 5; diag = 4 });
      Some (init_cell 2 None { col = 5; diag = 5 });
      Some (init_cell 1 None { col = 5; diag = 6 });
    |];
    [|
      None;
      None;
      None;
      Some (init_cell 1 None { col = 6; diag = 3 });
      Some (init_cell 1 None { col = 6; diag = 4 });
      Some (init_cell 1 None { col = 6; diag = 5 });
      Some (init_cell 1 None { col = 6; diag = 6 });
    |];
  |]

(** Requires: [coord] is a valid coordinate in the map (i.e. does not
    refer to a [None] cell) *)
let cell_at (map : t) coord : Cell.t option =
  let open HexUtil in
  map.(coord.col).(coord.diag)

let valid_coord (map : t) c = cell_at map c <> None

(** Requires: [coord] is a valid coordinate in the map (i.e. does not
    refer to a [None] cell) *)
let set_cell (map : t) cell coord : t =
  if valid_coord map coord then (
    let open HexUtil in
    map.(coord.col).(coord.diag) <- cell;
    map)
  else failwith "Invalid location"

let does_block (map : t) (d : HexUtil.dir) c1 c2 =
  let open HexUtil in
  match d with
  | 0 ->
      c1.col < c2.col && c1.diag < c2.diag
      && c2.col - c1.col = c2.diag - c2.col
  | 1 -> c1.col = c2.col && c2.diag > c1.diag
  | 2 -> c1.diag = c2.diag && c1.col > c2.col
  | 3 ->
      c1.col > c2.col && c1.diag > c2.diag
      && c2.col - c1.col = c2.diag - c2.col
  | 4 -> c1.col = c2.col && c1.diag > c2.diag
  | 5 -> c1.diag = c2.diag && c2.col > c1.col
  | _ -> failwith "Invalid direction"

let dist (map : t) c1 c2 =
  let open HexUtil in
  Int.abs (c1.col - c2.col) + Int.abs (c1.diag - c2.diag)

let neighbor (map : t) c (d : HexUtil.dir) =
  let open HexUtil in
  let new_coord =
    match d with
    | 0 -> { col = c.col + 1; diag = c.diag + 1 }
    | 1 -> { c with diag = c.diag + 1 }
    | 2 -> { c with col = c.col - 1 }
    | 3 -> { col = c.col - 1; diag = c.diag - 1 }
    | 4 -> { c with diag = c.diag - 1 }
    | 5 -> { c with col = c.col + 1 }
    | _ -> failwith "Invalid direction"
  in
  if valid_coord map new_coord then Some new_coord else None

let flatten (map : t) =
  let flat = ref [] in
  for col = 0 to Array.length map - 1 do
    for diag = 0 to Array.length map - 1 do
      match map.(col).(diag) with
      | Some c -> flat := c :: !flat
      | _ -> ()
    done
  done;
  !flat

let end_turn map =
  print_endline "Warning: Unimplemented";
  map
