(** The abstract type representing cells. *)
type t

(** The soil type of a cell *)
type soil = int

(** [init_cell soil plant] is a cell with soil type [soil]. If
    [plant = Some p], it will have plant [p]; if [plant = None], it will
    have no plant. *)
val init_cell : soil -> Plant.t option -> t

(** [coord cell] is the hex coordinate location of [cell]. *)
val coord : t -> HexUtil.coord

(** [soil cell] is the soil type of [cell]. *)
val soil : t -> soil

(** [plant cell] (is [Some Plant.t] in [cell] if [cell] contains a
    plant. Otherwise, it is [None]. ) *)
val plant : t -> Plant.t option

val string_of_cell : t -> string
