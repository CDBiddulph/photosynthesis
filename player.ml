open PlantInventory

type t = {
  id : PlayerId.t;
  light_points : int;
  score_points : int;
  store : Store.t;
  available : PlantInventory.t;
}

let max_lp = 20

let init_player id =
  {
    id;
    light_points = 0;
    score_points = 0;
    store = Store.init_store;
    available = PlantInventory.init_plant_inventory;
  }

let player_id player = player.id

let light_points player = player.light_points

let score_points player = player.score_points

let store player = player.store

let add_lp pts player =
  { player with light_points = min (player.light_points + pts) max_lp }

let add_sp pts player =
  { player with score_points = player.score_points + pts }

let is_in_available stage player =
  PlantInventory.num_remaining player.available stage > 0

let num_in_available stage player =
  PlantInventory.num_remaining player.available stage

let num_in_store player stage = Store.num_remaining player.store stage

let store_capacity player stage = Store.capacity stage

let is_store_full player stage =
  Store.remaining_capacity player.store stage = 0

let can_buy_plant player stage =
  num_in_store player stage > 0
  && player.light_points > Store.cost player.store stage

let buy_plant player stage =
  if can_buy_plant player stage then
    {
      player with
      store = Store.buy_plant player.store stage player.light_points;
      available = PlantInventory.add_plant player.available stage;
      light_points = player.light_points - Store.cost player.store stage;
    }
  else if num_in_store player stage = 0 then
    raise (PlantInventory.OutOfPlant stage)
  else
    raise
      (Store.InsufficientLightPoints (Store.cost player.store stage))
