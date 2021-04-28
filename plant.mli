(** A plant in the game. *)

(** The abstract type representing a plant. *)
type t

(** The type [plant_stage] represents the growth stage of a plant. *)
type plant_stage =
  | Seed
  | Small
  | Medium
  | Large

(** [init_plant player_id stage] is a plant [plant] where
    [player_id plant = player_id] and [plant_stage plant = stage]. *)
val init_plant : PlayerId.t -> plant_stage -> t

(** [player_id plant] is the [player_id] of the player who owns [plant]. *)
val player_id : t -> PlayerId.t

(** [plant_stage plant] is the [plant_stage] of [plant]. *)
val plant_stage : t -> plant_stage

(** [string_of_plant_stage plant_stage] is either "seed", "small",
    "medium", or "large", depending on [plant_stage]. *)
val string_of_plant_stage : plant_stage -> string

(** TODO *)
val int_of_plant_stage : plant_stage -> int

(** TODO *)
val next_stage : plant_stage -> plant_stage option

(** TODO *)
val all_stages : plant_stage list
