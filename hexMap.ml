(** The hexmap of the game, represented by a
    [Cell.t option array array]. *)

(** A 2-dimensional [Cell.t option list] to represent a hexagonal board
    with minimum width 4 and height 7. Lower indices (for purposes of
    representation and easier understanding) indicate closeness to the
    "top" and "left" side of of the board. "Columns" of [HexUtil.coord]
    are the first second in the [Cell.t option list list], and the
    "diagonals" are the first. *)
type t = Cell.t option list list

let init_map () : t =
  let open Yojson.Basic.Util in
  let open Cell in
  let j =
    Yojson.Basic.from_file "normal_map.json" |> member "map" |> to_list
  in
  List.map
    (fun col_json ->
      List.map
        (fun cell ->
          if member "opt" cell |> to_bool then
            let soil = member "soil" cell |> to_int in
            let col, diag =
              (member "col" cell |> to_int, member "diag" cell |> to_int)
            in
            Some (init_cell soil None { col; diag })
          else None)
        (to_list col_json))
    j

(** Requires: [coord] is a valid coordinate in the map (i.e. does not
    refer to a [None] cell) *)
let cell_at (map : t) coord : Cell.t option =
  let open HexUtil in
  try List.nth (List.nth map coord.col) coord.diag with _ -> None

let valid_coord (map : t) c = cell_at map c <> None

(* TODO: should prevent None set? Shouldn't ever happen, but would
   permanently reduce the board *)

(** Requires: [coord] is a valid coordinate in the map (i.e. does not
    refer to a [None] cell) *)
let set_cell (map : t) cell coord : t =
  if valid_coord map coord then
    let open HexUtil in
    List.map
      (fun col ->
        List.map
          (fun cell_opt ->
            match cell_opt with
            | None -> None
            | Some c -> if Cell.coord c = coord then cell else cell_opt)
          col)
      map
  else failwith "Invalid location"

let dist (map : t) c1 c2 =
  let open HexUtil in
  if
    Int.abs (c1.col - c2.col) = Int.abs (c1.diag - c2.diag)
    && ((c1.col > c2.col && c1.diag < c2.diag)
       || (c1.col < c2.col && c1.diag > c2.diag))
  then Int.abs (c1.col - c2.col) + Int.abs (c1.diag - c2.diag)
  else max (Int.abs (c1.col - c2.col)) (Int.abs (c1.diag - c2.diag))

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
  List.flatten map
  |> List.filter (fun c_opt ->
         match c_opt with None -> false | Some c -> true)
  |> List.map (fun c_opt ->
         match c_opt with None -> failwith "Unreachable" | Some c -> c)
