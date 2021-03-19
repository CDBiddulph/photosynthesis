(** The abstract type representing a plant. *)
type t

(** The type [plant_type] represents the *)
type plant_type =
  | Seed
  | Small
  | Medium
  | Large

val player_id : t -> Player.player_id

val plant_type : t -> plant_type

val to_string : t -> string