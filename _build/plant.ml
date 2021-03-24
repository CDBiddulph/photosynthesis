type plant_stage =
  | Seed
  | Small
  | Medium
  | Large

type t = {
  id : Player.player_id;
  stage : plant_stage;
}

let init_plant p_id plnt_st = { id = p_id; stage = plnt_st }

let player_id p = p.id

let plant_stage p = p.stage
