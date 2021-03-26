type ruleset =
  | Normal
  | Extended

(**  *)
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
  let first_player =
    match players with
    | [] -> failwith "Invalid player list"
    | hd :: tl -> hd
  in
  {
    players;
    map;
    sun_dir = sun;
    current_turn = Player.player_id first_player;
    current_phase = Photosynthesis;
    round_count = 0;
    rules = ruleset;
  }

let is_place_plant_legal board cell plant =
  HexMap.valid_coord board (Cell.coord cell) && Cell.plant cell = None

let place_plant (board : t) cell plant =
  if is_place_plant_legal board.map cell plant then
    let c = Cell.coord cell in
    HexMap.set_cell board.map
      (Cell.init_cell (Cell.soil cell) plant c)
      c
  else raise InvalidPlacement

(* TODO: need some way to increment life points/decrement light points *)

(** [lp_helper board player_cells player] returns an updated board with
    each player gaining the appropriate light points based on the sun's
    position. *)
let lp_helper board =
  let update_board = ref board in
  for i = 0 to List.length board.players - 1 do
    let player = List.nth board.players i in
    let player_cells =
      List.filter
        (fun c ->
          match Cell.plant c with
          | None -> false
          | Some plant ->
              Plant.player_id plant = Player.player_id player)
        (HexMap.flatten board.map)
    in
    ()
  done;
  !update_board

let end_turn board =
  (* TODO: light points *)
  let current_phase = board.current_phase in
  match current_phase with
  | Photosynthesis ->
      { (lp_helper board) with current_phase = Life_Cycle }
      (* let all_cells = HexMap.flatten board.map in let cur_player =
         List.nth board.players board.current_turn in let player_cells =
         List.filter (fun c -> match Cell.plant c with | None -> false |
         Some plant -> Plant.player_id plant = Player.player_id
         cur_player) all_cells in { board with current_phase =
         Life_Cycle } *)
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
