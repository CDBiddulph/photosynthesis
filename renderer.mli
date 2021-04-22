open HexUtil
open Raster

type t

val set_layer : string -> Raster.t -> t -> t

(** [get_graphic char_name color_name rend] is the combined char-color
    graphic with [char_name] from [rend.char_graphics] and [color_name]
    from [rend.color_graphics]. *)
val get_graphic : string -> string -> t -> Raster.t

val get_layer : string -> t -> Raster.t

(** [init_rend size layer_names char_grid_names color_grid_names] is a
    Renderer with the layers of rasters necessary to run the game.
    Postcondition: Each raster in [(init_rend cells).layers] has the
    same dimensions. *)
val init_rend :
  point2d -> string list -> string list -> string list -> t

val render : t -> unit

val blank_layer : t -> Raster.t

val fill_layer :
  t -> char option -> ANSITerminal.color option -> Raster.t
