(** Keeps track of the plants that one player can buy. *)

(* May want to internally represent this with a PlantInventory, along
   with other data structures to handle costs, capacities, etc. *)

(** Represents a store. *)
type t

exception InsufficientLightPoints of int

exception FullOfPlant of Plant.plant_stage

(** [init_store ()] is a new store with the default number of starting
    plants. *)
val init_store : t

(** [cost store stage] is the cost in light points to buy the next plant
    of [stage] in [store]. *)
val cost : t -> Plant.plant_stage -> int

(** [buy_plant store stage light_points] removes one plant of [stage]
    from [store]. Raises: [PlantInventory.OutOfPlant stage] if there are
    no more plants of [stage] in the store;
    [InsufficientLightPoints cost], where [cost] is the cost in light
    points of the next plant of [stage], if [light_points < cost]. *)
val buy_plant : t -> Plant.plant_stage -> int -> t

(** [add_plant store stage] adds one plant of [stage] to [store].
    Raises: [FullOfPlant stage] if no additional plants of [stage] can
    be added to [store]. *)
val add_plant : t -> Plant.plant_stage -> t

(** [num_remaining store stage] is the number of plants of [stage]
    remaining in [store]. *)
val num_remaining : t -> Plant.plant_stage -> int

(** [capacity store stage] is the maximum number of plants of [stage]
    that can be held at one time in [store]. *)
val capacity : Plant.plant_stage -> int

(** [remaining_capacity store stage] is the number empty slots of plants
    of [stage] in the [store] *)
val remaining_capacity : t -> Plant.plant_stage -> int
