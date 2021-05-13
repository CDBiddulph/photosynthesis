open PlantInventory
open Plant

(** A list of indices that point to the current cost of the next [Plant]
    of its type. The value at index 0 points to the cost of the next
    [Seed] which is in the int list at index 0 of costs, at index 1
    points to the cost of the next [Small] plant which is in the int
    list at index 1 in costs, and so on. The number of indices in [t]
    should always match the number of [Plant.plant_stage]s, and the
    value of the indices should be in bounds of its respective int list
    in costs. *)
type t = int list

exception InsufficientLightPoints of int

exception FullOfPlant of Plant.plant_stage

let init_store = [ 0; 0; 0; 0 ]

let costs = [ [ 1; 1; 2; 2 ]; [ 2; 2; 3; 3 ]; [ 3; 3; 4 ]; [ 4; 5 ] ]

let _init_store nums =
  List.map2
    (fun num c_row ->
      let capacity = List.length c_row in
      assert (capacity >= num);
      capacity - num)
    nums costs

let cost stage store =
  let ind = int_of_plant_stage stage in
  let stage_costs = List.nth costs ind in
  let remaining_of_stage = List.nth store ind in
  try List.nth stage_costs remaining_of_stage
  with Failure _ -> raise (PlantInventory.OutOfPlant stage)

let buy_plant stage light_points store =
  let cost_to_buy = cost stage store in
  if cost_to_buy > light_points then
    raise (InsufficientLightPoints cost_to_buy)
  else
    let ind = int_of_plant_stage stage in
    List.mapi
      (fun i count ->
        if i = ind then
          if count < List.length (List.nth costs ind) then count + 1
          else raise (OutOfPlant stage)
        else count)
      store

let add_plant stage store =
  let ind = int_of_plant_stage stage in
  List.mapi
    (fun i count ->
      if i = ind then
        if count - 1 >= 0 then count - 1 else raise (FullOfPlant stage)
      else count)
    store

let add_plant_if_not_full stage store =
  try add_plant stage store with FullOfPlant _ -> store

let num_remaining stage store =
  let ind = int_of_plant_stage stage in
  List.length (List.nth costs ind) - List.nth store ind

let capacity stage =
  List.nth costs (Plant.int_of_plant_stage stage) |> List.length

let remaining_capacity stage store =
  int_of_plant_stage stage |> List.nth store
