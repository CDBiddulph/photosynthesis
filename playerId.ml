type t = int

let first = 1

let generate_player_ids num_players =
  match num_players with
  | 2 -> [ 1; 2 ]
  | 3 -> [ 1; 2; 3 ]
  | 4 -> [ 1; 2; 3; 4 ]
  | _ -> failwith "Must be 2-4 players"
