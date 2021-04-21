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
  touched_cells : HexUtil.coord list;
}

exception IllegalPlacePlant

exception IllegalGrowPlant

exception IllegalHarvest

(** [plant_to_int plant] maps [Plant.plant_stage]s to light point
    amounts. *)
let plant_to_int (plant : Plant.plant_stage) : int =
  let open Plant in
  match plant with Seed -> 0 | Small -> 1 | Medium -> 2 | Large -> 3

let cell_at coord board = HexMap.cell_at board.map coord

let valid_coord coord board = HexMap.valid_coord board.map coord

let plant_at board coord =
  match cell_at board coord with
  | None -> None
  | Some cell -> Cell.plant cell

let init_board ruleset =
  { map = HexMap.init_map (); rules = ruleset; touched_cells = [] }

let testing_init_board ruleset cells =
  let board = init_board ruleset in
  let new_map =
    List.fold_left
      (fun map cell ->
        HexMap.set_cell map (Some cell) (Cell.coord cell))
      board.map cells
  in
  { board with map = new_map }

let cell_if_empty coord board =
  match cell_at board coord with
  | None -> None
  | Some c -> (
      match Cell.plant c with None -> Some c | Some p -> None)

(** [neighbors_in_dir board coord sun_dir] is the list of the three
    neighbors in direction [sun_dir]. If there are fewer than three
    legal neighbors, return only the legal neighbors. *)
let neighbors_in_dir board coord sun_dir =
  match HexMap.neighbor board.map coord sun_dir with
  | None -> []
  | Some fst_neigh_coord ->
      fst_neigh_coord
      ::
      (match HexMap.neighbor board.map fst_neigh_coord sun_dir with
      | None -> []
      | Some snd_neigh_coord ->
          snd_neigh_coord
          ::
          (match HexMap.neighbor board.map snd_neigh_coord sun_dir with
          | None -> []
          | Some thd_neigh_coord -> [ thd_neigh_coord ]))

(** [within_radius p_id coord board] checks if there is a plant owned by
    the player with [p_id] within a proper radius of [coord]. *)
let within_radius p_id coord board =
  let valid_dirs = [ 0; 1; 2; 3; 4; 5 ] in
  let all_neighbors =
    List.flatten
      (List.map
         (fun dir -> neighbors_in_dir board coord dir)
         valid_dirs)
  in
  let open Plant in
  List.fold_left
    (* check if has plant owned by player; if not, false; if so, match
       plant size -> allowed distance *)
      (fun acc neighbor_coord ->
      match cell_at neighbor_coord board with
      | None -> failwith "Should never happen"
      | Some cell -> (
          match Cell.plant cell with
          | None -> acc
          | Some plant ->
              if Plant.player_id plant = p_id then
                acc
                || HexMap.dist board.map neighbor_coord coord
                   <= plant_to_int (plant_stage plant)
              else acc))
    false all_neighbors

let can_plant_seed player_id coord board =
  (not (List.mem coord board.touched_cells))
  && cell_if_empty board coord <> None
  && within_radius player_id coord board

(** [can_plant_small coord board] is [true] if there is an empty cell at
    [coord] in [board] and the cell has soil of type [1]. *)
let can_plant_small coord board =
  (not (List.mem coord board.touched_cells))
  &&
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
          touched_cells = coord :: board.touched_cells;
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
  (not (List.mem coord board.touched_cells))
  &&
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
          touched_cells = coord :: board.touched_cells;
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

(** [player_lp_helper board player_cells] returns an association list of
    [HexUtil.coord]s where the player's plants are and their respective
    light point values based on [board]'s sun direction.*)
let player_lp_helper sun_dir (player_cells : HexUtil.coord list) board :
    (HexUtil.coord * int) list =
  let single_cell_shadowed coord =
    let neighbors = neighbors_in_dir board coord sun_dir in
    let shadowed =
      List.fold_left
        (fun acc neighbors_coord ->
          shadows board.map neighbors_coord coord || acc)
        false neighbors
    in
    if shadowed then None
    else
      let plt_stage =
        match cell_at coord board with
        | None -> failwith "should be impossible"
        | Some cell -> (
            match Cell.plant cell with
            | None -> failwith "should be impossible"
            | Some plant -> Plant.plant_stage plant)
      in
      Some (plant_to_int plt_stage)
  in
  List.fold_left
    (fun acc player_cell_coord ->
      match single_cell_shadowed player_cell_coord with
      | None -> acc
      | Some lp_value -> (player_cell_coord, lp_value) :: acc)
    [] player_cells

let get_photo_lp sun_dir players board =
  let get_player_cell_coords player_id =
    List.map
      (fun cell -> Cell.coord cell)
      (List.filter
         (fun cell ->
           match Cell.plant cell with
           | None -> false
           | Some plant -> Plant.player_id plant = player_id)
         (HexMap.flatten board.map))
  in
  List.map
    (fun player_id ->
      ( player_id,
        player_lp_helper sun_dir
          (get_player_cell_coords player_id)
          board ))
    players

let can_harvest player c board =
  (not (List.mem c board.touched_cells))
  &&
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
        {
          board with
          map = new_map;
          touched_cells = coord :: board.touched_cells;
        }
  else raise IllegalHarvest

let cells (board : t) : Cell.t list = HexMap.flatten board.map

let end_turn board = { board with touched_cells = [] }

let actionable_cells board =
  List.filter
    (fun coord -> not (List.mem coord board.touched_cells))
    (List.map (fun cell -> Cell.coord cell) (cells board))
