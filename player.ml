type player_id = int

(* add player board (available trees to buy) *)
type t = {
  id : player_id;
  light_points : int;
  score_points : int;
  store : Store.t;
  available : PlantInventory.t;
}

let buy_plant player stage =
  {
    player with
    store = Store.buy_plant player.store stage player.light_points;
  }

let player_id player = player.id

let light_points player = player.light_points

let score_points player = player.score_points

let add_lp player pts =
  { player with light_points = player.light_points + pts }

let add_sp player pts =
  { player with score_points = player.score_points + pts }
