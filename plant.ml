type plant_stage =
  | Seed
  | Small
  | Medium
  | Large

type t = {
  id : PlayerId.t;
  render_char : char;
  render_color : ANSITerminal.color;
  stage : plant_stage;
}

let init_plant p_id ch col plnt_st =
  { id = p_id; render_char = ch; render_color = col; stage = plnt_st }

let player_id p = p.id

let render_char p = p.render_char

let render_color p = p.render_color

let plant_stage p = p.stage

let string_of_plant_stage stage =
  match stage with
  | Seed -> "seed"
  | Small -> "small"
  | Medium -> "medium"
  | Large -> "large"
