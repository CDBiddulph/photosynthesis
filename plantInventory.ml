open Plant

exception OutOfPlant of Plant.plant_stage

exception InvalidNumber of int

type t = Plant.plant_stage list

let init_plant_inventory =
  [ Plant.Seed; Plant.Seed; Plant.Small; Plant.Small; Plant.Medium ]

let rec init_plant_inventory_gen
    (stage : Plant.plant_stage)
    (x : int)
    (acc : t) =
  match x with
  | 0 -> acc
  | _ ->
      let acc = stage :: acc in
      let num = x - 1 in
      init_plant_inventory_gen stage num acc

let empty = []

let is_empty inv = inv = empty

let size inv = List.length inv

let remove_bool1 (x : Plant.t) (y : Plant.plant_stage) : bool =
  Plant.plant_stage x <> y

let remove_bool2 (x : int) = match x with 0 -> true | _ -> false

let rec remove_helper inv stage acc x =
  match inv with
  | [] -> acc
  | h :: t ->
      if h = stage && x = 0 then remove_helper t stage acc x
      else
        let acc = h :: acc in
        let x = 1 in
        remove_helper t stage acc x

let remove_plant inv stage =
  let acc = empty in
  let x = 0 in
  if List.mem stage inv then remove_helper inv stage acc x
  else raise (OutOfPlant stage)

let add_plant inv stage = stage :: inv

let num_remaining inv stage =
  let new_t = List.filter (fun x -> x = stage) inv in
  List.length new_t
