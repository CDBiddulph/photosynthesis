type t = {
  id : PlayerId.t;
  light_points : int;
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
