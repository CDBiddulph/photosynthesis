(** A plant in the game. *)

(** The abstract type representing a plant. *)
type t

(** Raised when access to a non-existant stage is attempted. *)
exception StageDoesNotExist

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

(** [int_of_plant_stage stage] maps a plant stage to an int.
    [Plant.Seed] maps to 0, [Plant.Small] maps to 1, and so on. *)
val int_of_plant_stage : plant_stage -> int

(** [next_stage stage] is the plant stage following [stage]. Raises:
    [StageDoesNotExist] if [stage] is [Plant.Large]. *)
val next_stage : plant_stage -> plant_stage option

(** [all_stages] is the list of all possible plant stages. *)
val all_stages : plant_stage list

(** [last_stage stage] is the stage before [stage]. Raises:
    [StageDoesNotExist] if [stage = Seed]. *)
val last_stage : plant_stage -> plant_stage

(** [next_stage stage] is the stage after [stage]. Raises:
    [StageDoesNotExist] if [stage = Large]. *)
val next_stage : plant_stage -> plant_stage
