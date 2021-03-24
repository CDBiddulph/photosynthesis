type t = {
  players : Player.t list;
  map : HexMap.t;
  sun_dir : HexUtil.dir;
  current_turn : Player.player_id;
  current_stage : int;
}

exception InvalidPlacement

let init_game players map sun =
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
    current_stage = 0;
  }

let is_place_plant_legal board cell plant =
  HexMap.valid_coord board (Cell.coord cell) && Cell.plant cell = None

let place_plant board cell plant =
  if is_place_plant_legal board cell plant then
    let c = Cell.coord cell in
    HexMap.set_cell board.map
      (Cell.init_cell (Cell.soil cell) plant c)
      c
  else raise InvalidPlacement

let end_turn board =
  (* partial implementation; does not account for sun movements or
     number of turns yet *)
  let rec current_player_ind = function
    | [] -> 0
    | hd :: tl ->
        if Player.player_id hd = board.current_turn then 0
        else 1 + current_player_ind tl
  in
  let cur =
    current_player_ind board.players mod List.length board.players
  in
  let rec get_ind i = function
    | [] -> None
    | hd :: tl -> if i = 0 then Some hd else get_ind (i - 1) tl
  in
  match get_ind cur board.players with
  | None -> board
  | Some next_player ->
      { board with current_turn = Player.player_id next_player }

let sun_dir board = board.sun_dir
