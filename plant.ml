type plant_stage =
  | Seed
  | Small
  | Medium
  | Large

type t = {
  id : PlayerId.t;
  stage : plant_stage;
}

let init_plant p_id plnt_st = { id = p_id; stage = plnt_st }

let player_id p = p.id

let plant_stage p = p.stage

let string_of_plant_stage stage =
  match stage with
  | Seed -> "seed"
  | Small -> "small"
  | Medium -> "medium"
  | Large -> "large"
