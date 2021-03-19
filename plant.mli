(** The abstract type representing a plant. *)
type t

(** The type [plant_stage] represents the growth stage of a plant. *)
type plant_stage =
  | Seed
  | Small
  | Medium
  | Large

(** [player_id plant] is the [player_id] of the player who owns [plant]. *)
val player_id : t -> Player.player_id

(** [plant_stage plant] is the [plant_stage] of [plant]. *)
val plant_stage : t -> plant_stage

(** [to_string plant] is the string representation of plant. TODO: flesh
    out this definition. *)
val to_string : t -> string
