(** Keeps track of a collection of different stages of plants. *)

(** Represents an available area. *)
type t

(** Raised when attempting to remove a plant type when there are none of
    that type left. *)
exception OutOfPlant of Plant.plant_stage

(** [init_plant_inventory] is a new plant inventory with the default
    number of starting plants. *)
val init_plant_inventory : t

(** [init_plant_inventory_gen stage num inv] is a new plant inventory
    with x amount of plant with stage [stage] *)
val init_plant_inventory_gen : Plant.plant_stage -> int -> t -> t

(** [empty] is an empty plant inventory. *)
val empty : t

(** [is_empty inv] returns true if the inventory is empty and false if
    the inventory contains elements. *)
val is_empty : t -> bool

(** [size inv] is the number of plants in [inv] *)
val size : t -> int

(** [remove_plant stage inv] removes one plant of [stage] from
    [plant_inventory]. Raises: [OutOfPlant stage] if there are no more
    plants of [stage] in [inv]. *)
val remove_plant : Plant.plant_stage -> t -> t

(** [add_plant stage inv] adds one plant of [stage] to [inv]. *)
val add_plant : Plant.plant_stage -> t -> t

(** [num_remaining stage inv] is the number of plants of [stage]
    remaining in [inv]. *)
val num_remaining : Plant.plant_stage -> t -> int
