open PlantInventory

type order_key = int

type t = (order_key * PlantInventory.t) list

exception InsufficientLightPoints of int

exception FullOfPlant of Plant.plant_stage

exception OutOfPlant of Plant.plant_stage

exception InvalidOrderKey of order_key

let init_store_gen (order : order_key) (inv : PlantInventory.t) : t =
  [ (order, inv) ]

let init_store =
  init_store_gen 1
    (init_plant_inventory_gen Plant.Seed 2 PlantInventory.empty)
  @ init_store_gen 2
      (init_plant_inventory_gen Plant.Seed 2 PlantInventory.empty)
  @ init_store_gen 3
      (init_plant_inventory_gen Plant.Small 2 PlantInventory.empty)
  @ init_store_gen 4
      (init_plant_inventory_gen Plant.Small 2 PlantInventory.empty)
  @ init_store_gen 5
      (init_plant_inventory_gen Plant.Medium 2 PlantInventory.empty)
  @ init_store_gen 6
      (init_plant_inventory_gen Plant.Medium 1 PlantInventory.empty)
  @ init_store_gen 7
      (init_plant_inventory_gen Plant.Large 1 PlantInventory.empty)
  @ init_store_gen 8
      (init_plant_inventory_gen Plant.Large 1 PlantInventory.empty)

let sort store : t = List.sort (fun (x, _) (y, _) -> compare x y) store

let find_list (store : t) (stage : Plant.plant_stage) =
  match stage with
  | Plant.Seed ->
      if PlantInventory.is_empty (List.assoc 1 store) = false then
        List.nth store 0
      else if
        PlantInventory.is_empty (List.assoc 1 store)
        && PlantInventory.is_empty (List.assoc 2 store) = false
      then List.nth store 1
      else raise (OutOfPlant stage)
  | Plant.Small ->
      if PlantInventory.is_empty (List.assoc 3 store) = false then
        List.nth store 2
      else if
        PlantInventory.is_empty (List.assoc 3 store)
        && PlantInventory.is_empty (List.assoc 4 store) = false
      then List.nth store 3
      else raise (OutOfPlant stage)
  | Plant.Medium ->
      if PlantInventory.is_empty (List.assoc 5 store) = false then
        List.nth store 4
      else if
        PlantInventory.is_empty (List.assoc 5 store)
        && PlantInventory.is_empty (List.assoc 6 store) = false
      then List.nth store 5
      else raise (OutOfPlant stage)
  | Plant.Large ->
      if PlantInventory.is_empty (List.assoc 7 store) = false then
        List.nth store 6
      else if
        PlantInventory.is_empty (List.assoc 7 store)
        && PlantInventory.is_empty (List.assoc 8 store) = false
      then List.nth store 7
      else raise (OutOfPlant stage)

let ordkey (x : order_key * PlantInventory.t) = match x with y, _ -> y

let inv (x : order_key * PlantInventory.t) = match x with _, y -> y

let cost_order (x : order_key) =
  match x with
  | 1 -> 1
  | 2 -> 2
  | 3 -> 2
  | 4 -> 3
  | 5 -> 3
  | 6 -> 4
  | 7 -> 4
  | 8 -> 5
  | _ -> raise (InvalidOrderKey x)

let cost store stage = find_list store stage |> ordkey |> cost_order

let buy_plant store stage light_points =
  try
    if light_points >= cost store stage then
      let lst = find_list store stage in
      let order = ordkey lst in
      let new_inv = PlantInventory.remove_plant (inv lst) stage in
      let new_bracket = init_store_gen order new_inv in
      sort (new_bracket @ List.remove_assoc order store)
    else store
  with OutOfPlant stage -> store

let plant_is_full (stage : Plant.plant_stage) (store : t) : bool =
  match stage with
  | Plant.Seed ->
      if PlantInventory.size (inv (List.nth store 0)) = 2 then true
      else false
  | Plant.Small ->
      if PlantInventory.size (inv (List.nth store 2)) = 2 then true
      else false
  | Plant.Medium ->
      if PlantInventory.size (inv (List.nth store 4)) = 2 then true
      else false
  | Plant.Large ->
      if PlantInventory.size (inv (List.nth store 6)) = 2 then true
      else false

let add_plant store stage =
  try
    let lst = find_list store stage in
    let order = ordkey lst in
    let new_inv =
      if plant_is_full stage store = false then
        PlantInventory.add_plant (inv lst) stage
      else raise (FullOfPlant stage)
    in
    let new_bracket = init_store_gen order new_inv in
    sort (new_bracket @ List.remove_assoc order store)
  with OutOfPlant stage -> store

let num_remaining store stage =
  let order = find_list store stage |> ordkey in
  match order with
  | 1 -> PlantInventory.size (inv (find_list store stage))
  | 2 -> 2 + PlantInventory.size (inv (find_list store stage))
  | 3 -> PlantInventory.size (inv (find_list store stage))
  | 4 -> 2 + PlantInventory.size (inv (find_list store stage))
  | 5 -> PlantInventory.size (inv (find_list store stage))
  | 6 -> 2 + PlantInventory.size (inv (find_list store stage))
  | 7 -> PlantInventory.size (inv (find_list store stage))
  | 8 -> 1 + PlantInventory.size (inv (find_list store stage))
  | _ -> raise (InvalidOrderKey order)

let capacity stage =
  match stage with
  | Plant.Seed -> 4
  | Plant.Small -> 4
  | Plant.Medium -> 3
  | Plant.Large -> 2

let remaining_capacity store stage =
  let order = find_list store stage |> ordkey in
  match order with
  | 1 -> 2 - PlantInventory.size (inv (find_list store stage))
  | 2 -> 4 - PlantInventory.size (inv (find_list store stage))
  | 3 -> 2 - PlantInventory.size (inv (find_list store stage))
  | 4 -> 4 - PlantInventory.size (inv (find_list store stage))
  | 5 -> 2 - PlantInventory.size (inv (find_list store stage))
  | 6 -> 1 - PlantInventory.size (inv (find_list store stage))
  | 7 -> 1 - PlantInventory.size (inv (find_list store stage))
  | 8 -> 2 - PlantInventory.size (inv (find_list store stage))
  | _ -> raise (InvalidOrderKey order)
