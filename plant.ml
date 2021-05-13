open PlayerId

type plant_stage =
  | Seed
  | Small
  | Medium
  | Large

type t = {
  id : PlayerId.t;
  stage : plant_stage;
}

exception StageDoesNotExist

let init_plant p_id plnt_st = { id = p_id; stage = plnt_st }

let player_id p = p.id

let plant_stage p = p.stage

let string_of_plant_stage stage =
  match stage with
  | Seed -> "seed"
  | Small -> "small"
  | Medium -> "medium"
  | Large -> "large"

let int_of_plant_stage stage =
  match stage with Seed -> 0 | Small -> 1 | Medium -> 2 | Large -> 3

let last_stage stage =
  match stage with
  | Seed -> raise StageDoesNotExist
  | Small -> Seed
  | Medium -> Small
  | Large -> Medium

let next_stage stage =
  match stage with
  | Seed -> Small
  | Small -> Medium
  | Medium -> Large
  | Large -> raise StageDoesNotExist

let all_stages = [ Seed; Small; Medium; Large ]
