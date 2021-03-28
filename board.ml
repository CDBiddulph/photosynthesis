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
  players : Player.t list;
  map : HexMap.t;
  sun_dir : HexUtil.dir;
  current_turn : int;
  current_phase : round_phase;
  round_count : int;
  rules : ruleset;
}

exception InvalidPlacement

let init_game players map sun ruleset =
  {
    players;
    map;
    sun_dir = sun;
    current_turn = 0;
    current_phase = Life_Cycle;
    round_count = 0;
    rules = ruleset;
  }

let is_place_plant_legal board cell plant =
  HexMap.valid_coord board.map (Cell.coord cell)
  && Cell.plant cell = None

let place_plant (board : t) cell plant =
  if is_place_plant_legal board cell plant then
    let c = Cell.coord cell in
    {
      board with
      map =
        HexMap.set_cell board.map
          (Cell.init_cell (Cell.soil cell) (Some plant) c)
          c;
    }
  else raise InvalidPlacement

(* TODO: need some way to increment score points/decrement light points *)

(** [shadows c1 c2] determines if the [Plant.t] in [c1] would shadow the
    [Plant.t] in [c2] based on size and distance. *)
let shadows map c1 c2 =
  let open Cell in
  let open Plant in
  let c1_cell_opt = HexMap.cell_at map c1 in
  let c2_cell_opt = HexMap.cell_at map c2 in
  match c1_cell_opt with
  | None -> false
  | Some cell_1 -> (
      match c2_cell_opt with
      | None -> false
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

(** [player_lp_helper board player player_cells] returns an updated
    board with only [player]'s light points updated based on the sun's
    position. *)
let player_lp_helper (board : t) player player_cells : t =
  let update_board = ref board in
  for i = 0 to List.length player_cells - 1 do
    let cell_coord = List.nth player_cells i in
    let shadowed =
      let fst_neigh_opt =
        HexMap.neighbor board.map cell_coord board.sun_dir
      in
      match fst_neigh_opt with
      | None -> false
      | Some fst_coord -> (
          let snd_neigh =
            HexMap.neighbor board.map fst_coord board.sun_dir
          in
          match snd_neigh with
          | None -> shadows board.map fst_coord cell_coord
          | Some snd_coord -> (
              let thd_neigh =
                HexMap.neighbor board.map snd_coord board.sun_dir
              in
              match thd_neigh with
              | None ->
                  shadows board.map fst_coord cell_coord
                  || shadows board.map snd_coord cell_coord
              | Some thd_coord ->
                  shadows board.map fst_coord cell_coord
                  || shadows board.map snd_coord cell_coord
                  || shadows board.map thd_coord cell_coord))
    in
    if not shadowed then
      let (Some cell) = HexMap.cell_at board.map cell_coord in
      let plnt = Cell.plant cell in
      match plnt with
      | None -> ()
      | Some lp_plnt ->
          let lp = lp_map (Plant.plant_stage lp_plnt) in
          let new_plst =
            List.map
              (fun ply ->
                if Player.player_id ply = player then
                  Player.add_lp ply lp
                else ply)
              board.players
          in
          update_board := { !update_board with players = new_plst }
    else ()
  done;
  !update_board

(** [lp_helper board player_cells player] returns an updated board with
    each player gaining the appropriate light points based on the sun's
    position. *)
let lp_helper (board : t) : t =
  let update_board = ref board in
  (* let opposite_sun_dir = (board.sun_dir + 3) mod 6 in *)
  for i = 0 to List.length board.players - 1 do
    let player = List.nth board.players i in
    let player_cells =
      List.map
        (fun c -> Cell.coord c)
        (List.filter
           (fun c ->
             match Cell.plant c with
             | None -> false
             | Some plant ->
                 Plant.player_id plant = Player.player_id player)
           (HexMap.flatten board.map))
    in
    update_board :=
      player_lp_helper !update_board
        (Player.player_id player)
        player_cells
  done;
  !update_board

let end_turn (board : t) : t =
  (* TODO: In progress: light points *)
  let current_phase = board.current_phase in
  match current_phase with
  | Photosynthesis ->
      { (lp_helper board) with current_phase = Life_Cycle }
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

let can_remove board = false

let remove_plant board = board
