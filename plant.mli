(** The abstract type representing a plant. *)
type t

(** The type [plant_stage] represents the growth stage of a plant. *)
type plant_stage =
  | Seed
  | Small
  | Medium
  | Large

(** [init_plant p s] is a plant with [player_id] [p] and [plant_stage]
    [s]. *)
val init_plant : Player.player_id -> plant_stage -> t

(** [player_id plant] is the [player_id] of the player who owns [plant]. *)
val player_id : t -> Player.player_id

(** [plant_stage plant] is the [plant_stage] of [plant]. *)
val plant_stage : t -> plant_stage

(** [string_of_plant plant] is the string representation of [plant].
    TODO: flesh out this definition. *)
val string_of_plant : t -> string
