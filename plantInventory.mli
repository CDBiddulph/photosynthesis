(** Keeps track of a collection of different stages of plants. *)

(** Represents an available area. *)
type t

exception OutOfPlant of Plant.plant_stage

(** [init_plant_inventory ()] is a new plant inventory with the default
    number of starting plants. *)
val init_plant_inventory : t

(** [remove_plant plant_inventory stage] removes one plant of [stage]
    from [plant_inventory]. Raises: [OutOfPlant stage] if there are no
    more plants of [stage] in [plant_inventory]. *)
val remove_plant : t -> Plant.plant_stage -> int -> t

(** [add_plant plant_inventory stage] adds one plant of [stage] to
    [plant_inventory]. *)
val add_plant : t -> Plant.plant_stage -> t

(** [num_remaining plant_inventory stage] is the number of plants of
    [stage] remaining in [plant_inventory]. *)
val num_remaining : t -> Plant.plant_stage -> int
