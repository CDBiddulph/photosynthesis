type plant

type plant_type =
  | Seed
  | Small
  | Medium
  | Large

val player_id : plant -> int

val p_type : plant -> plant_type
