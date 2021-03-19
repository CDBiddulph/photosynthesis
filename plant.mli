(** The abstract type representing a plant. *)
type t

(**  *)
type plant_type =
  | Seed
  | Small
  | Medium
  | Large

val player_id : t -> Player.player_id

val plant_type : t -> plant_type
