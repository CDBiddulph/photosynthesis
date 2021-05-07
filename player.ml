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

let is_in_available stage player =
  PlantInventory.num_remaining stage player.available > 0

let num_in_available stage player =
  PlantInventory.num_remaining stage player.available

let num_in_store stage player = Store.num_remaining stage player.store

let store_capacity stage player = Store.capacity stage

let is_store_full stage player =
  Store.remaining_capacity stage player.store = 0

(** [cost_to_grow stage] is the cost in light points to grow a plant to
    [stage] from the stage before [stage], if it exists. *)
let cost_to_grow stage =
  let open Plant in
  match stage with
  | Seed -> failwith "Cannot \"grow to\" a Seed"
  | Small -> 1
  | Medium -> 2
  | Large -> 3

let cost_to_harvest = 4

let can_buy_plant stage player =
  num_in_store stage player > 0
  && player.light_points >= Store.cost stage player.store

(* Planting, i.e. putting a plant on an empty cell, never costs light
   points *)
let can_plant_plant stage player = num_in_available stage player > 0

let can_grow_plant stage player =
  num_in_available stage player > 0
  && player.light_points >= cost_to_grow stage

let can_harvest player = player.light_points >= cost_to_harvest

let buy_plant stage player =
  let cost = Store.cost stage player.store in
  if can_buy_plant stage player then
    {
      player with
      store = Store.buy_plant stage player.light_points player.store;
      available = PlantInventory.add_plant stage player.available;
      light_points = player.light_points - cost;
    }
  else if num_in_store stage player <= 0 then
    raise (PlantInventory.OutOfPlant stage)
  else raise (Store.InsufficientLightPoints cost)

let plant_plant stage player =
  if can_plant_plant stage player then
    {
      player with
      available = PlantInventory.remove_plant stage player.available;
    }
  else raise (PlantInventory.OutOfPlant stage)

let grow_plant stage player =
  let cost = cost_to_grow stage in
  if can_grow_plant stage player then
    {
      player with
      store =
        Store.add_plant_if_not_full
          (Plant.last_stage stage)
          player.store;
      available = PlantInventory.remove_plant stage player.available;
      light_points = player.light_points - cost;
    }
  else if num_in_available stage player <= 0 then
    raise (PlantInventory.OutOfPlant stage)
  else raise (Store.InsufficientLightPoints cost)

let harvest sp player =
  if can_harvest player then
    {
      player with
      store = Store.add_plant_if_not_full Large player.store;
      score_points = player.score_points + sp;
      light_points = player.light_points - cost_to_harvest;
    }
  else raise (Store.InsufficientLightPoints cost_to_harvest)

let _set_available available player = { player with available }

let _available player = player.available

let _set_store store player = { player with store }

let _store player = player.store
