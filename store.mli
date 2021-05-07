(** The plant store for each player. *)

(** Represents a store. *)
type t

exception InsufficientLightPoints of int

(** Raised when a plant is added to a store without capacity for it. *)
exception FullOfPlant of Plant.plant_stage

(** [init_store] is a new store with the default number of starting
    plants. *)
val init_store : t

(** [_init_store nums] is a store with the plant quantities of [nums].
    Precondition: each num is less than the corresponding capacity. *)
val _init_store : int list -> t

(** [costs] is the list of costs for plants. *)
val costs : int list list

(** [cost stage] is the cost in light points to buy the next plant of
    [stage]. *)
val cost : Plant.plant_stage -> t -> int

(** [buy_plant stage light_points store] removes one plant of [stage]
    from [store]. Raises: [PlantInventory.OutOfPlant stage] if there are
    no more plants of [stage] in the store;
    [Player.InsufficientLightPoints] if [light_points] is less than the
    amount necessary to buy a plant of [stage]. *)
val buy_plant : Plant.plant_stage -> int -> t -> t

(** [add_plant stage store] adds one plant of [stage] to [store].
    Raises: [FullOfPlant stage] if no additional plants of [stage] can
    be added to [store]. *)
val add_plant : Plant.plant_stage -> t -> t

(** [num_remaining stage store] is the number of plants of [stage]
    remaining in [store]. *)
val num_remaining : Plant.plant_stage -> t -> int

(** [capacity stage] is the maximum number of plants of [stage] that can
    be held at one time. *)
val capacity : Plant.plant_stage -> int

(** [remaining_capacity stage store] is the number empty slots of plants
    of [stage] in the [store] *)
val remaining_capacity : Plant.plant_stage -> t -> int
