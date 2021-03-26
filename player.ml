type player_id = int

type t = {
  id : player_id;
  light_points : int;
  score_points : int;
}

let player_id player = player.id

let light_points player = player.light_points

let score_points player = player.score_points

let add_lp player pts =
  { player with light_points = player.light_points + pts }

let add_sp player pts =
  { player with score_points = player.score_points + pts }
