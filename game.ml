(** TODO: Doc comment *)
type t = {
  players : (PlayerId.t * Player.t) list;
  board : Board.t;
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
    board = Board.init_board ruleset;
  }

let player_of game id = List.assoc id game.players

let update_board game board = { game with board }

let update_players game players = { game with players }

let place_plant game coord plant =
  update_board game (Board.place_plant game.board coord plant)

let harvest game player_id coord =
  update_board game (Board.harvest game.board player_id coord)

let buy_plant game player_id stage = failwith "Not Implemented"

let end_turn game = failwith "Not Implemented"

let is_place_plant_legal game coord plant =
  Board.is_place_plant_legal game.board coord plant

let num_in_available game player_id stage = failwith "Not Implemented"

let num_in_store game player_id stage = failwith "Not Implemented"

let store_capacity game player_id stage = failwith "Not Implemented"

let is_store_full game player_id stage = failwith "Not Implemented"

let turn game = failwith "Not Implemented"

let player_of game player_id = failwith "Not Implemented"

let cell_at game coord = failwith "Not Implemented"

let can_buy_plant game player_id stage = failwith "Not Implemented"

let player_light_points game player_id = failwith "Not Implemented"

let player_scoring_points game player_id = failwith "Not Implemented"

let next_scoring_points game soil = failwith "Not Implemented"

let cells game = failwith "Not Implemented"
