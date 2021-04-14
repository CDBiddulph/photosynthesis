type ruleset =
  | Normal
  | Extended

(** The type [round_phase] represents the part of the round. *)
type round_phase =
  | Photosynthesis
  | Life_Cycle

(** A record type representing a game and its data. [sun_dir] indicates
    the direction that shadows are cast. *)
type t = {
  map : HexMap.t;
  rules : ruleset;
}

exception IllegalPlacePlant

exception IllegalGrowPlant

exception IllegalHarvest

let cell_at coord board = HexMap.cell_at board.map coord

let valid_coord coord board = HexMap.valid_coord board.map coord

let plant_at board coord =
  match cell_at board coord with
  | None -> None
  | Some cell -> Cell.plant cell

let init_board ruleset = { map = HexMap.init_map (); rules = ruleset }

let cell_if_empty coord board =
  match cell_at board coord with
  | None -> None
  | Some c -> (
      match Cell.plant c with None -> Some c | Some p -> None)

(* TODO: check that location is within the necessary radius of one of
   the player's trees *)
let can_plant_seed player_id coord board =
  cell_if_empty board coord <> None

(** [can_plant_small coord board] is [true] if there is an empty cell at
    [coord] in [board] and the cell has soil of type [1]. *)
let can_plant_small coord board =
  match cell_if_empty board coord with
  | None -> false
  | Some c -> Cell.soil c = 1

let place_plant can_place plant coord board =
  if can_place then
    match cell_at coord board with
    | None -> failwith "Unreachable"
    | Some old_cell ->
        {
          board with
          map =
            HexMap.set_cell board.map
              (Some (Cell.set_plant old_cell (Some plant)))
              coord;
        }
  else raise IllegalPlacePlant

let plant_seed player_id coord board =
  place_plant
    (can_plant_seed player_id coord board)
    (Plant.init_plant player_id Plant.Seed)
    coord board

let plant_small player_id coord board =
  place_plant
    (can_plant_small coord board)
    (Plant.init_plant player_id Plant.Small)
    coord board

let can_grow_plant player_id coord board =
  match plant_at coord board with
  | None -> false
  | Some old_plant ->
      let old_player_id = Plant.player_id old_plant in
      let not_large = Plant.plant_stage old_plant <> Plant.Large in
      old_player_id = player_id && not_large

let grow_plant coord player_id board =
  if can_grow_plant player_id coord board then
    match cell_at coord board with
    | None -> failwith "Impossible"
    | Some old_cell ->
        let old_plant =
          match Cell.plant old_cell with
          | None -> failwith "Impossible"
          | Some p -> p
        in
        let next_plant =
          Some
            (Plant.init_plant player_id
               (match
                  old_plant |> Plant.plant_stage |> Plant.next_stage
                with
               | None -> failwith "Impossible"
               | Some p -> p))
        in
        {
          board with
          map =
            HexMap.set_cell board.map
              (Some (Cell.set_plant old_cell next_plant))
              coord;
        }
  else raise IllegalGrowPlant

(** [shadows map c1 c2] determines if the [Plant.t] in [c1] would shadow
    the [Plant.t] in [c2] based on size and distance. *)
let shadows map c1 c2 =
  let open Cell in
  let open Plant in
  let c1_cell_opt = HexMap.cell_at map c1 in
  let c2_cell_opt = HexMap.cell_at map c2 in
  match c1_cell_opt with
  | None -> false
  | Some cell_1 -> (
      match c2_cell_opt with
      | None -> true
      | Some cell_2 -> (
          let c1_plnt_opt = plant cell_1 in
          let c2_plnt_opt = plant cell_2 in
          match c2_plnt_opt with
          | None -> true
          | Some c2_plt -> (
              let c2_plnt = plant_stage c2_plt in
              c2_plnt = Seed
              ||
              match c1_plnt_opt with
              | None -> false
              | Some c1_plt -> (
                  let c1_plnt = plant_stage c1_plt in
                  match c1_plnt with
                  | Seed -> false
                  | Small ->
                      c2_plnt = Small && HexMap.dist map c1 c2 = 1
                  | Medium ->
                      (c2_plnt = Medium || c2_plnt = Small)
                      && HexMap.dist map c1 c2 <= 2
                  | Large ->
                      (c2_plnt = Large || c2_plnt = Medium
                     || c2_plnt = Small)
                      && HexMap.dist map c1 c2 <= 3))))

(** [lp_map plant] maps [Plant.plant_stage]s to light point amounts. *)
let lp_map (plant : Plant.plant_stage) : int =
  let open Plant in
  match plant with Seed -> 0 | Small -> 1 | Medium -> 2 | Large -> 3

(** [player_lp_helper board player_cells] returns an association list of
    [HexUtil.coord]s where the player's plants are and their respective
    light point values based on [board]'s sun direction.*)
let player_lp_helper sun_dir (player_cells : HexUtil.coord list) board :
    (HexUtil.coord * int) list =
  let coord_lp_lst = ref [] in
  for i = 0 to List.length player_cells - 1 do
    let cell_coord = List.nth player_cells i in
    let shadowed =
      let fst_neigh_opt =
        HexMap.neighbor board.map cell_coord sun_dir
      in
      match fst_neigh_opt with
      | None -> false
      | Some fst_coord -> (
          let fst_shadow = shadows board.map fst_coord cell_coord in
          let snd_neigh = HexMap.neighbor board.map fst_coord sun_dir in
          match snd_neigh with
          | None -> fst_shadow
          | Some snd_coord -> (
              let snd_shadow =
                fst_shadow || shadows board.map snd_coord cell_coord
              in
              let thd_neigh =
                HexMap.neighbor board.map snd_coord sun_dir
              in
              match thd_neigh with
              | None -> snd_shadow
              | Some thd_coord ->
                  snd_shadow || shadows board.map thd_coord cell_coord))
    in
    if not shadowed then
      match cell_at cell_coord board with
      | None -> failwith "should be a valid cell"
      | Some cell -> (
          let plnt = Cell.plant cell in
          match plnt with
          | None -> ()
          | Some lp_plnt ->
              let lp = lp_map (Plant.plant_stage lp_plnt) in
              coord_lp_lst := (cell_coord, lp) :: !coord_lp_lst)
  done;
  !coord_lp_lst

let get_photo_lp sun_dir players board =
  let out = ref [] in
  for i = 0 to List.length players - 1 do
    let player = List.nth players i in
    let player_cells =
      List.map
        (fun c -> Cell.coord c)
        (List.filter
           (fun c ->
             match Cell.plant c with
             | None -> false
             | Some plant -> Plant.player_id plant = player)
           (HexMap.flatten board.map))
    in
    out := (player, player_lp_helper sun_dir player_cells board) :: !out
  done;
  !out

let can_harvest player c board =
  match cell_at c board with
  | None -> false
  | Some cell -> (
      match Cell.plant cell with
      | None -> false
      | Some plnt -> (
          match Plant.plant_stage plnt with
          | Plant.Large -> Plant.player_id plnt = player
          | _ -> false))

let harvest player_id coord board =
  if can_harvest player_id coord board then
    match cell_at coord board with
    | None -> failwith "Unreachable"
    | Some cell ->
        let new_cell =
          Some (Cell.init_cell (Cell.soil cell) None coord)
        in
        let new_map = HexMap.set_cell board.map new_cell coord in
        { board with map = new_map }
  else raise IllegalHarvest

let cells (board : t) : Cell.t list = HexMap.flatten board.map

(* TODO: some notion of turn to keep track of cells that are touched
   this "turn" *)
(* TODO: update shadows/lp to use blocks instead of what's happening now
   (?) *)
