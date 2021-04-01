(** Keeps track of a collection of different stages of plants. *)

(** Represents an available area. *)
type t

exception OutOfPlant of Plant.plant_stage

(** [init_available ()] is a new available area with the default number
    of starting plants. *)
val init_available : t

(** [remove_plant available stage] removes one plant of [stage] from
    [available]. Raises: [OutOfPlant stage] if there are no more plants
    of [stage] in [available]. *)
val remove_plant : t -> Plant.plant_stage -> int -> t

(** [add_plant available stage] adds one plant of [stage] to
    [available]. *)
val add_plant : t -> Plant.plant_stage -> t

(** [num_remaining available stage] is the number of plants of [stage]
    remaining in [store]. *)
val num_remaining : t -> Plant.plant_stage -> int
