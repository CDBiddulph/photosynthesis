open Plant

exception OutOfPlant of Plant.plant_stage

(** An association list of [Plant.plant_stage]s to the number of the
    respective plants in the inventory. There should always be an entry
    in the list for each stage. *)
type t = (Plant.plant_stage * int) list

let init_plant_inventory =
  [
    (Plant.Seed, 2);
    (Plant.Small, 4);
    (Plant.Medium, 1);
    (Plant.Large, 0);
  ]

let _init_plant_inventory (nums : (Plant.plant_stage * int) list) : t =
  nums

let _contents (inv : t) : (Plant.plant_stage * int) list = inv

let init_plant_inventory_gen stage num inv =
  List.map
    (fun (inv_stage, count) ->
      if inv_stage = stage then (inv_stage, num) else (inv_stage, count))
    inv

let empty =
  [
    (Plant.Seed, 0);
    (Plant.Small, 0);
    (Plant.Medium, 0);
    (Plant.Large, 0);
  ]

let size inv = List.fold_left (fun acc (_, count) -> acc + count) 0 inv

let is_empty inv = size inv = 0

let remove_plant stage inv =
  List.map
    (fun (inv_stage, count) ->
      if inv_stage = stage then
        if count > 0 then (inv_stage, count - 1)
        else raise (OutOfPlant inv_stage)
      else (inv_stage, count))
    inv

let add_plant stage inv =
  List.map
    (fun (inv_stage, count) ->
      if inv_stage = stage then (inv_stage, count + 1)
      else (inv_stage, count))
    inv

let num_remaining stage inv =
  let rem =
    List.filter (fun (inv_stage, count) -> inv_stage = stage) inv
  in
  match rem with
  | [] -> failwith "invalid stage"
  | [ (_, count) ] -> count
  | hd :: tl -> failwith "invalid inventory representation"
