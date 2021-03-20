type player_id = int

type t = {
  id : player_id;
  light_points : int;
}

let player_id player = player.id

let light_points player = player.light_points
