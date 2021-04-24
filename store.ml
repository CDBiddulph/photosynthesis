open PlantInventory

type t = (Plant.plant_stage * int * int list) list

exception InsufficientLightPoints of int

exception FullOfPlant of Plant.plant_stage

exception OutOfPlant of Plant.plant_stage

let init_store =
  let open Plant in
  [
    (Seed, 0, [ 1; 1; 2; 2 ]);
    (Small, 0, [ 2; 2; 3; 3 ]);
    (Medium, 0, [ 3; 3; 4 ]);
    (Large, 0, [ 4; 5 ]);
  ]

let rec cost store stage =
  match store with
  | [] -> failwith "invalid stage"
  | (store_stage, ind, costs) :: tl ->
      if store_stage = stage then List.nth costs ind else cost tl stage

let buy_plant store stage light_points =
  if cost store stage > light_points then
    raise (InsufficientLightPoints light_points)
  else
    List.fold_right
      (fun (store_stage, ind, costs) new_store ->
        if store_stage = stage then
          if ind + 1 < List.length costs then
            (store_stage, ind + 1, costs) :: new_store
          else raise (FullOfPlant stage)
        else (store_stage, ind, costs) :: new_store)
      store []

let rec plant_is_full stage store =
  match store with
  | [] -> failwith "invalid stage"
  | (store_stage, ind, costs) :: tl ->
      store_stage = stage && ind = List.length costs

let add_plant store stage =
  List.fold_right
    (fun (store_stage, ind, costs) new_store ->
      if store_stage = stage then
        if ind - 1 >= 0 then (store_stage, ind - 1, costs) :: new_store
        else raise (OutOfPlant stage)
      else new_store)
    store []

let num_remaining store stage =
  let entry =
    List.filter (fun (store_stage, _, _) -> store_stage = stage) store
  in
  match entry with
  | [] -> failwith "invalid stage"
  | [ (store_stage, ind, costs) ] -> List.length costs - ind
  | _ -> failwith "invalid store representation"

let capacity stage =
  match stage with
  | Plant.Seed -> 4
  | Plant.Small -> 4
  | Plant.Medium -> 3
  | Plant.Large -> 2

let remaining_capacity store stage =
  let entry =
    List.filter (fun (store_stage, _, _) -> store_stage = stage) store
  in
  match entry with
  | [] -> failwith "invalid stage"
  | [ (store_stage, ind, costs) ] -> ind
  | _ -> failwith "invalid store representation"
