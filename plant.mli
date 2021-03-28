(** The abstract type representing a plant. *)
type t

(** The type [plant_stage] represents the growth stage of a plant. *)
type plant_stage =
  | Seed
  | Small
  | Medium
  | Large

(** [init_plant player ch col stage] is a plant [plant] where
    [player_id plant = player], [render_char plant = ch],
    [render_color plant = col], and [plant_stage plant = stage]. *)
val init_plant :
  Player.player_id -> char -> ANSITerminal.color -> plant_stage -> t

(** [player_id plant] is the [player_id] of the player who owns [plant]. *)
val player_id : t -> Player.player_id

(** [render_char plant] is the lowercase version of the character used
    to represent [plant] when it is rendered in the GUI. *)
val render_char : t -> char

(** [render_color plant] is the color used to represent [plant] when it
    is rendered in the GUI. *)
val render_color : t -> ANSITerminal.color

(** [plant_stage plant] is the [plant_stage] of [plant]. *)
val plant_stage : t -> plant_stage

(** [string_of_plant_stage plant_stage] is either "seed", "small",
    "medium", or "large", depending on [plant_stage]. *)
val string_of_plant_stage : plant_stage -> string
