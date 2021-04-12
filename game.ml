type t = {
  players : (PlayerId.t * Player.t) list;
  player_order : PlayerId.t list;
  board : Board.t;
  turn : PlayerId.t;
  starting_turn : PlayerId.t;
  setup_rounds_left : int;
  scoring_points : (Cell.soil * int list) list;
  num_rounds : int;
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
    setup_rounds_left = 2;
    scoring_points =
      [
        (1, [ 14; 14; 13; 13; 13; 12; 12; 12; 12 ]);
        (2, [ 17; 16; 16; 14; 14; 13; 13 ]);
        (3, [ 19; 18; 18; 17; 17 ]);
        (4, [ 22; 21; 20 ]);
      ];
    num_rounds = 0;
  }

let player_of game id = List.assoc id game.players

let turn game = game.turn

let player_of_turn game = game |> turn |> player_of game

let update_board board game = { game with board }

let update_players players game = { game with players }

let update_player player_id player game =
  update_players
    ((player_id, player) :: List.remove_assoc player_id game.players)
    game

let grow_plant game player_id coord =
  update_board (Board.grow_plant game.board coord player_id) game

let plant_seed game coord player_id =
  update_board (Board.plant_seed game.board coord player_id) game

(** [get_scoring_points game soil] is a tuple [(sp, new_game)] where
    [sp] is the number of scoring points awarded to the next player to
    harvest a tree on [soil] and [new_game] is [game] with the
    corresponding scoring token removed. *)
let rec get_scoring_points game soil =
  try
    match List.assoc soil game.scoring_points with
    | [] -> get_scoring_points game (soil - 1)
    | h :: t ->
        ( h,
          {
            game with
            scoring_points =
              (soil, t) :: List.remove_assoc soil game.scoring_points;
          } )
  with Not_found -> (0, game)

let cell_at game coord =
  match Board.cell_at game.board coord with
  | None -> failwith "invalid cell"
  | Some cell -> cell

let harvest game player_id coord =
  let harvest_game =
    update_board (Board.harvest game.board player_id coord) game
  in
  (* Should already have failed if harvesting is not possible *)
  let sp_to_add, scored_game =
    get_scoring_points harvest_game
      (coord |> cell_at harvest_game |> Cell.soil)
  in
  let scored_player =
    player_id |> player_of game |> Player.add_sp sp_to_add
  in
  scored_game |> update_player player_id scored_player

let buy_plant game stage =
  let player = player_of game game.turn in
  update_player game.turn (Player.buy_plant player stage) game

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
      (player_id, Player.add_lp lp (player_of game player_id)))
    lp_per_player

let end_turn_normal turn_after_this is_new_round game =
  let new_starting_turn =
    if is_new_round then turn_after game game.starting_turn
    else game.starting_turn
  in
  let new_turn =
    if is_new_round then new_starting_turn else turn_after_this
  in
  let new_players =
    if is_new_round then photosynthesis game else game.players
  in
  let new_board =
    if is_new_round then Board.move_sun game.board else game.board
  in
  let new_num_rounds =
    game.num_rounds + if is_new_round then 1 else 0
  in
  {
    game with
    starting_turn = new_starting_turn;
    turn = new_turn;
    players = new_players;
    board = new_board;
    num_rounds = new_num_rounds;
  }

let end_turn_setup turn_after_this is_new_round game =
  let new_setup_rounds_left =
    game.setup_rounds_left - if is_new_round then 1 else 0
  in
  {
    game with
    turn = turn_after_this;
    setup_rounds_left = new_setup_rounds_left;
  }

let end_turn game =
  let turn_after_this = turn_after game game.turn in
  let is_new_round = turn_after_this = game.starting_turn in
  if game.setup_rounds_left = 0 then
    end_turn_normal turn_after_this is_new_round game
  else end_turn_setup turn_after_this is_new_round game

let can_plant_seed game coord player_id =
  Board.can_plant_seed game.board player_id coord

let can_grow_plant game coord player_id =
  Board.can_grow_plant game.board player_id coord

let next_scoring_points game soil = fst (get_scoring_points game soil)

let cells game = Board.cells game.board

let sun_dir game = HexUtil.dir_of_int game.num_rounds
