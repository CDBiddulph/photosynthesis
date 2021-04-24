open PlantInventory

(** TODO *)
type t = int list

exception InsufficientLightPoints of int

exception FullOfPlant of Plant.plant_stage

exception OutOfPlant of Plant.plant_stage

let init_store =
  let open Plant in
  [ 0; 0; 0; 0 ]

let costs = [ [ 1; 1; 2; 2 ]; [ 2; 2; 3; 3 ]; [ 3; 3; 4 ]; [ 4; 5 ] ]

(** TODO *)
let stage_to_ind =
  let open Plant in
  function Seed -> 0 | Small -> 1 | Medium -> 2 | Large -> 3

let cost store stage =
  let ind = stage_to_ind stage in
  List.nth (List.nth costs ind) (List.nth store ind)

let buy_plant store stage light_points =
  if cost store stage > light_points then
    raise (InsufficientLightPoints light_points)
  else
    let ind = stage_to_ind stage in
    List.mapi
      (fun i count ->
        if i = ind then
          if count + 1 < List.length (List.nth costs ind) then count + 1
          else raise (OutOfPlant stage)
        else count)
      store

let plant_is_full stage store =
  let ind = stage_to_ind stage in
  List.nth store ind = 0

let add_plant store stage =
  let ind = stage_to_ind stage in
  List.mapi
    (fun i count ->
      if i = ind then
        if count - 1 >= 0 then count - 1 else raise (FullOfPlant stage)
      else count)
    store

let num_remaining store stage =
  let ind = stage_to_ind stage in
  List.length (List.nth costs ind) - List.nth store ind

let capacity stage =
  match stage with
  | Plant.Seed -> 4
  | Plant.Small -> 4
  | Plant.Medium -> 3
  | Plant.Large -> 2

let remaining_capacity store stage =
  stage_to_ind stage |> List.nth store
