type t = {
  players : Player.t list;
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
    players = List.map Player.init_player player_ids;
    board = Board.init_board ruleset;
  }
