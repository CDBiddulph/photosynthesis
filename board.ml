type ruleset =
  | Normal
  | Extended

(** The type [round_phase] represents the part of the round. *)
type round_phase =
  | Photosynthesis
  | Life_Cycle

(** A record type representing a game and its data. The [Player.t list]
    is a list of all the players in the game, with the turn order being
    the order of players in the list. [current_phase] indicates if it is
    a Photosynthesis (true) or Life Cycle (false) phase. *)
type t = {
  map : HexMap.t;
  sun_dir : HexUtil.dir;
  current_phase : round_phase;
  round_count : int;
  rules : ruleset;
  (* will be moved into the game layer *)
  players : Player.t list;
  current_turn : int;
}

exception InvalidPlacement

let init_game players map sun ruleset =
  {
    map;
    sun_dir = sun;
    current_phase = Life_Cycle;
    round_count = 0;
    rules = ruleset;
    (* will be moved *)
    players;
    current_turn = 0;
  }

let is_place_plant_legal board c plant =
  HexMap.valid_coord board.map c
  &&
  match HexMap.cell_at board.map c with
  | None -> false
  | Some cell -> (
      match Cell.plant cell with
      | None -> true
      | Some old_plt -> (
          let old_player = Plant.player_id old_plt in
          let new_player = Plant.player_id plant in
          old_player = new_player
          &&
          let new_stage = Plant.plant_stage plant in
          match Plant.plant_stage old_plt with
          | Plant.Seed -> new_stage = Plant.Small
          | Plant.Small -> new_stage = Plant.Medium
          | Plant.Medium -> new_stage = Plant.Large
          | Plant.Large -> false))

let place_plant (board : t) c plant =
  if is_place_plant_legal board c plant then
    match HexMap.cell_at board.map c with
    | None -> failwith "should be a valid cell"
    | Some cell ->
        {
          board with
          map =
            HexMap.set_cell board.map
              (Some (Cell.init_cell (Cell.soil cell) (Some plant) c))
              c;
        }
  else raise InvalidPlacement

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
let player_lp_helper (board : t) (player_cells : HexUtil.coord list) :
    (HexUtil.coord * int) list =
  let coord_lp_lst = ref [] in
  for i = 0 to List.length player_cells - 1 do
    let cell_coord = List.nth player_cells i in
    let shadowed =
      let fst_neigh_opt =
        HexMap.neighbor board.map cell_coord board.sun_dir
      in
      match fst_neigh_opt with
      | None -> false
      | Some fst_coord -> (
          let fst_shadow = shadows board.map fst_coord cell_coord in
          let snd_neigh =
            HexMap.neighbor board.map fst_coord board.sun_dir
          in
          match snd_neigh with
          | None -> fst_shadow
          | Some snd_coord -> (
              let snd_shadow =
                fst_shadow || shadows board.map snd_coord cell_coord
              in
              let thd_neigh =
                HexMap.neighbor board.map snd_coord board.sun_dir
              in
              match thd_neigh with
              | None -> snd_shadow
              | Some thd_coord ->
                  snd_shadow || shadows board.map thd_coord cell_coord))
    in
    if not shadowed then
      match HexMap.cell_at board.map cell_coord with
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

let get_photo_lp board players =
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
    out := (player, player_lp_helper board player_cells) :: !out
  done;
  !out

let end_turn (board : t) : t =
  let current_phase = board.current_phase in
  match current_phase with
  | Photosynthesis ->
      { board with current_phase = Life_Cycle }
      (* { (lp_helper board) with current_phase = Life_Cycle } *)
  | Life_Cycle ->
      if board.current_turn = List.length board.players - 1 then
        {
          board with
          current_phase = Photosynthesis;
          current_turn = 0;
          sun_dir = (board.sun_dir + 1) mod 6;
          round_count = board.round_count + 1;
        }
      else { board with current_turn = board.current_turn + 1 }

let sun_dir board = board.sun_dir

let flat_board (board : t) : Cell.t list = HexMap.flatten board.map

let can_remove board c =
  match HexMap.cell_at board.map c with
  | None -> false
  | Some cell -> (
      match Cell.plant cell with
      | None -> false
      | Some plnt -> (
          match Plant.plant_stage plnt with
          | Plant.Large -> true
          | _ -> false))

let remove_plant board c =
  if can_remove board c then
    match HexMap.cell_at board.map c with
    | None -> failwith "should be a valid cell"
    | Some cell ->
        let new_cell = Some (Cell.init_cell (Cell.soil cell) None c) in
        let new_map = HexMap.set_cell board.map new_cell c in
        { board with map = new_map }
  else board
