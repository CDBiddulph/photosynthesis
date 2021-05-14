(* open Graph.Flow *)
open Map
open Flow

type ruleset =
  | Normal
  | Shadows

module OrderedCoord = struct
  open HexUtil

  type t = HexUtil.coord

  let hash c = (c.col * 10) + c.diag

  let compare x y = compare (hash x) (hash y)
end

module PlantTrackMap = Map.Make (OrderedCoord)

(** A record type representing a game and its data. [sun_dir] indicates
    the direction that shadows are cast. *)
type t = {
  map : HexMap.t;
  rules : ruleset;
  touched_cells : HexUtil.coord list;
  n_planted : int;
  graph : BaseGraph.t;
  src_sink : HexUtil.coord * HexUtil.coord;
}

exception IllegalPlacePlant

exception IllegalGrowPlant

exception IllegalHarvest

(** [plant_to_int plant] maps [Plant.plant_stage]s to light point
    amounts. *)
let plant_to_int (plant : Plant.plant_stage) : int =
  let open Plant in
  match plant with Seed -> 0 | Small -> 1 | Medium -> 2 | Large -> 3

let init_board ruleset =
  {
    map = HexMap.init_map ();
    rules = ruleset;
    touched_cells = [];
    n_planted = 0;
    graph = BaseGraph.empty;
    src_sink = ({ col = -1; diag = 0 }, { col = 0; diag = -1 });
  }

let testing_init_board ruleset cells =
  let board = init_board ruleset in
  let new_map =
    List.fold_left
      (fun map cell ->
        HexMap.set_cell map (Some cell) (Cell.coord cell))
      board.map cells
  in
  { board with map = new_map }

let map board = board.map

let ruleset board = board.rules

let cells (board : t) : Cell.t list = HexMap.flatten board.map

let cell_at coord board = HexMap.cell_at board.map coord

let valid_coord coord board = HexMap.valid_coord board.map coord

let plant_at board coord =
  match cell_at board coord with
  | None -> None
  | Some cell -> Cell.plant cell

let get_all_trees board id =
  List.filter
    (fun cell ->
      match Cell.plant cell with
      | None -> false
      | Some plant -> Plant.player_id plant = id)
    (cells board)
  |> List.map (fun cell -> Cell.coord cell)

(** [cell_if_empty coord board] is the [Cell.t option] of the cell at
    [coord]. If the cell has a plant or is invalid, returns [None]. *)
let cell_if_empty coord board =
  match cell_at board coord with
  | None -> None
  | Some c -> (
      match Cell.plant c with None -> Some c | Some p -> None)

(** [neighbors_in_radius board coord radius] gets all legal neighbors of
    [coord] within [radius] of [coord]. *)
let neighbors_in_radius board coord radius =
  List.map
    (fun cell -> Cell.coord cell)
    (List.filter
       (fun cell ->
         HexMap.dist board.map (Cell.coord cell) coord <= radius)
       (HexMap.flatten board.map))

(** [within_radius p_id coord board] checks if there is a plant owned by
    the player with [p_id] within a proper radius of [coord]. *)
let within_radius p_id coord board =
  let all_neighbors = neighbors_in_radius board coord 3 in
  let open Plant in
  List.fold_left
    (* check if has plant owned by player; if not, false; if so, match
       plant size -> allowed distance *)
      (fun acc neighbor_coord ->
      match plant_at neighbor_coord board with
      | None -> acc
      | Some plant ->
          if Plant.player_id plant = p_id then
            acc
            || HexMap.dist board.map neighbor_coord coord
               <= plant_to_int (plant_stage plant)
          else acc)
    false all_neighbors

(** TODO *)
let add_all seed cands graph (src, sink) =
  let with_seed =
    BaseGraph.add_edge (BaseGraph.add_vertex graph seed) src seed
  in
  List.fold_left
    (fun acc cand ->
      BaseGraph.add_edge
        (BaseGraph.add_edge (BaseGraph.add_vertex acc cand) seed cand)
        cand sink)
    with_seed cands

(** TODO *)
let valid_plant seed cands board =
  let g = board.graph in
  let g_with = add_all seed cands g board.src_sink in
  let src, sink = board.src_sink in
  let _, n = MaxFlow.maxflow g_with src sink in
  n = board.n_planted + 1

(** TODO *)
let get_usable_neighbors player_id coord board =
  let neighbors = neighbors_in_radius board coord 3 in
  List.filter
    (fun c ->
      match plant_at c board with
      | None -> false
      | Some p ->
          Plant.player_id p = player_id
          && (not (List.mem c board.touched_cells))
          && HexMap.dist board.map c coord
             <= plant_to_int (Plant.plant_stage p))
    neighbors

let can_plant_seed player_id coord board =
  let usable_neighbors = get_usable_neighbors player_id coord board in
  (not (List.mem coord board.touched_cells))
  && valid_plant coord usable_neighbors board
  && cell_if_empty board coord <> None
  && within_radius player_id coord board

(** [can_plant_small coord board] is [true] if there is an empty cell at
    [coord] in [board] and the cell has soil of type [1]. Should only be
    called in setup phase. *)
let can_plant_small coord board =
  match cell_if_empty board coord with
  | None -> false
  | Some c -> Cell.soil c = 1

(** [place_plant can_place plant coord board] places [plant] at [coord]
    on [board] if [can_place] is true, otherwise raises
    [IllegalPlacePlant]. *)
let place_plant can_place plant coord board =
  if can_place then
    match cell_at coord board with
    | None -> failwith "Unreachable"
    | Some old_cell ->
        let usable_neighbors =
          get_usable_neighbors (Plant.player_id plant) coord board
        in
        let new_graph =
          add_all coord usable_neighbors board.graph board.src_sink
        in
        {
          board with
          map =
            HexMap.set_cell board.map
              (Some (Cell.set_plant old_cell (Some plant)))
              coord;
          touched_cells = coord :: board.touched_cells;
          graph = new_graph;
          n_planted = board.n_planted + 1;
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
  && (board.n_planted = 0
     ||
     let src, sink = board.src_sink in
     let _, n =
       MaxFlow.maxflow
         (BaseGraph.remove_vertex board.graph coord)
         src sink
     in
     n = board.n_planted)
  &&
  match plant_at coord board with
  | None -> false
  | Some old_plant ->
      let old_player_id = Plant.player_id old_plant in
      let not_large = Plant.plant_stage old_plant <> Plant.Large in
      old_player_id = player_id && not_large

let grow_plant player_id coord board =
  if can_grow_plant player_id coord board then
    let old_cell =
      match cell_at coord board with
      | None -> failwith "should not happen"
      | Some c -> c
    in
    let old_plant =
      match plant_at coord board with
      | None -> failwith "Impossible"
      | Some p -> p
    in
    let next_plant =
      Some
        (Plant.init_plant player_id
           (old_plant |> Plant.plant_stage |> Plant.next_stage))
    in
    let new_graph = BaseGraph.remove_vertex board.graph coord in
    {
      board with
      map =
        HexMap.set_cell board.map
          (Some (Cell.set_plant old_cell next_plant))
          coord;
      touched_cells = coord :: board.touched_cells;
      graph = new_graph;
    }
  else raise IllegalGrowPlant

(** [shadows map c1 c2] determines if the [Plant.t] in [c1] would shadow
    the [Plant.t] in [c2] based on size and distance. *)
let shadows board c1 c2 =
  let open Cell in
  let open Plant in
  let c1_plnt_opt = plant_at c1 board in
  let c2_plnt_opt = plant_at c2 board in
  match c2_plnt_opt with
  | None -> true
  | Some c2_plt -> (
      let c2_plnt = plant_stage c2_plt in
      c2_plnt = Seed
      ||
      match c1_plnt_opt with
      | None -> false
      | Some c1_plt ->
          let c1_plnt = plant_stage c1_plt in
          HexMap.dist board.map c1 c2 <= plant_to_int c1_plnt
          && plant_to_int c2_plnt <= plant_to_int c1_plnt)

(** [neighbors_in_dir_r board coord sun_dir dist] is the list of the
    [dist] neighbors in direction [sun_dir]. If there are fewer than
    [dist] legal neighbors, return only the legal neighbors. *)
let rec neighbors_in_dir_r board coord dir dist =
  if dist = 0 then []
  else
    match HexMap.neighbor board.map coord dir with
    | None -> []
    | Some neigh_coord ->
        neigh_coord
        :: neighbors_in_dir_r board neigh_coord dir (dist - 1)

(** [player_lp_helper board player_cells] returns an association list of
    [HexUtil.coord]s where the player's plants are and their respective
    light point values based on [board]'s sun direction.*)
let player_lp_helper sun_dir (player_cells : HexUtil.coord list) board :
    (HexUtil.coord * int) list =
  let single_cell_shadowed coord =
    let neighbors =
      neighbors_in_dir_r board coord (HexUtil.reverse_dir sun_dir) 3
    in
    let shadowed =
      List.fold_left
        (fun acc neighbors_coord ->
          shadows board neighbors_coord coord || acc)
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
  && (board.n_planted = 0
     ||
     let src, sink = board.src_sink in
     let _, n =
       MaxFlow.maxflow (BaseGraph.remove_vertex board.graph c) src sink
     in
     n = board.n_planted)
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

let end_turn board =
  {
    board with
    touched_cells = [];
    graph = BaseGraph.empty;
    n_planted = 0;
  }

let actionable_cells board =
  List.filter
    (fun coord -> not (List.mem coord board.touched_cells))
    (List.map (fun cell -> Cell.coord cell) (cells board))
