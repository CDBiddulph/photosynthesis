open PlantInventory

type t = {
  id : PlayerId.t;
  light_points : int;
  score_points : int;
  store : Store.t;
  available : PlantInventory.t;
}

let init_player id =
  {
    id;
    light_points = 0;
    score_points = 0;
    store = Store.init_store;
    available = PlantInventory.init_plant_inventory;
  }

let buy_plant player stage =
  {
    player with
    store = Store.buy_plant player.store stage player.light_points;
  }

let player_id player = player.id

let light_points player = player.light_points

let score_points player = player.score_points

let add_lp pts player =
  { player with light_points = player.light_points + pts }

let add_sp pts player =
  { player with score_points = player.score_points + pts }

let store t = t.store

let is_in_available stage player = failwith "Unimplemented"

let num_in_available stage player = PlantInventory.num_remaining player.available stage

let num_in_store player stage = Store.num_remaining player.store stage

let store_capacity player stage = failwith "Unimplemented"

let is_store_full player stage = failwith "Unimplemented"

let can_buy_plant player stage = failwith "Unimplemented"
