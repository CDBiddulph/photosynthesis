(** The abstract type representing cells. *)
type t

(** The type of soil types *)
type soil_type = int

(** The type of board coordinates *)
val coord : HexMap.coord

(** [soil_type cell] is the soil type of [cell]. *)
val soil_type : t -> soil_type

(** [plant cell] (is [Some Plant.t] in [cell] if [cell] contains a
    plant. Otherwise, it is [None]. ) *)
val plant : t -> Plant.t option

val to_string : t -> string
