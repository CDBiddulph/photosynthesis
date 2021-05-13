(** A cell in the hexmap of the game. *)

(** The abstract type representing cells. *)
type t

(** The soil type of a cell *)
type soil = int

(** [init_cell soil plant coord] is a cell with soil type [soil]. If
    [plant = Some p], it will have plant [p]; if [plant = None], it will
    have no plant. *)
val init_cell : soil -> Plant.t option -> HexUtil.coord -> t

(** [coord cell] is the hex coordinate location of [cell]. *)
val coord : t -> HexUtil.coord

(** [soil cell] is the soil type of [cell]. *)
val soil : t -> soil

(** [plant cell] (is [Some Plant.t] in [cell] if [cell] contains a
    plant. Otherwise, it is [None]. ) *)
val plant : t -> Plant.t option

(** [set_plant cell plant] is [cell] with plant [plant]. *)
val set_plant : t -> Plant.t option -> t
