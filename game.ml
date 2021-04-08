type t = {
  players : (PlayerId.t * Player.t) list;
  player_order : PlayerId.t list;
  board : Board.t;
  turn : PlayerId.t;
  starting_turn : PlayerId.t;
  is_setup : bool;
}

let init_game num_players ruleset =
  let player_ids =
    match num_players with
    | 2 -> [ 1; 2 ]
    | 3 -> [ 1; 2; 3 ]
    | 4 -> [ 1; 2; 3; 4 ]
    | _ -> failwith "Must be 2-4 players"
  in
  {
    players =
      List.map (fun id -> (id, Player.init_player id)) player_ids;
    player_order = player_ids;
    board = Board.init_board ruleset;
    turn = List.nth player_ids 0;
    starting_turn = List.nth player_ids 0;
    is_setup = true;
  }

let player_of game id = List.assoc id game.players

let turn game = game.turn

let update_board game board = { game with board }

let update_players game players = { game with players }

let update_player game player_id player =
  update_players game
    ((player_id, player) :: List.remove_assoc player_id game.players)

let place_plant game plant coord =
  assert (Plant.player_id plant = game.turn);
  update_board game (Board.place_plant game.board coord plant)

let harvest game coord =
  update_board game (Board.harvest game.board game.turn coord)

let buy_plant game stage =
  let player = player_of game game.turn in
  update_player game game.turn (Player.buy_plant player stage)

let next_in_wraparound_lst lst elem =
  let rec next_in_wraparound_lst_helper first lst elem =
    match lst with
    | [] -> failwith "Element of list not found"
    | [ h ] ->
        if h = elem then first else failwith "Element of list not found"
    | h :: next :: t ->
        if h = elem then next
        else next_in_wraparound_lst_helper first (next :: t) elem
  in
  next_in_wraparound_lst_helper (List.nth lst 0) lst elem

let turn_after game player =
  next_in_wraparound_lst game.player_order player

(** [photosynthesis game] returns a list of [player_id]s and their
    respective [players] from [game], with their light_point values
    updated according to the positions of their plants and the rules of
    the photosynthesis stage. *)
let photosynthesis game =
  let add_lp coord_lp_lst =
    List.fold_left (fun sum (_, lp) -> sum + lp) 0 coord_lp_lst
  in
  let lp_lst = Board.get_photo_lp game.board game.player_order in
  let lp_per_player =
    List.map
      (fun (player_id, coord_lp_lst) ->
        (player_id, add_lp coord_lp_lst))
      lp_lst
  in
  List.map
    (fun (player_id, lp) ->
      (player_id, Player.add_lp (player_of game player_id) lp))
    lp_per_player

let end_turn game =
  (* TODO: implement rules for game.is_setup = true*)
  let turn_after_this = turn_after game game.turn in
  let will_photo = turn_after_this = game.starting_turn in
  let new_starting_turn =
    if will_photo then turn_after game game.starting_turn
    else game.starting_turn
  in
  let new_turn =
    if will_photo then new_starting_turn else turn_after_this
  in
  let new_players =
    if will_photo then photosynthesis game else game.players
  in
  let new_board =
    if will_photo then Board.move_sun game.board else game.board
  in
  {
    game with
    starting_turn = new_starting_turn;
    turn = new_turn;
    players = new_players;
    board = new_board;
  }

let is_place_plant_legal game coord plant =
  Board.is_place_plant_legal game.board coord game.turn plant

let is_plant_in_available game stage = failwith "Not Implemented"

let num_in_available game stage = failwith "Not Implemented"

let num_in_store game stage = failwith "Not Implemented"

let store_capacity game stage = failwith "Not Implemented"

let is_store_full game stage = failwith "Not Implemented"

let turn game = failwith "Not Implemented"

let cell_at game coord = failwith "Not Implemented"

let can_buy_plant game stage = failwith "Not Implemented"

let player_light_points game player_id =
  player_of game player_id |> Player.light_points

let player_scoring_points game player_id =
  player_of game player_id |> Player.score_points

let next_scoring_points game soil = failwith "Not Implemented"

let cells game = Board.cells game.board
