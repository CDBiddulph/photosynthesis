open HexUtil
open Raster
open ANSITerminal

type t

val set_layer : string -> Raster.t -> t -> t

(** [get_graphic char_name color_name rend] is the combined char-color
    graphic with [char_name] from [rend.char_graphics] and [color_name]
    from [rend.color_graphics]. *)
val get_graphic_with_color_grid : string -> string -> t -> Raster.t

(** [get_graphic char_name color rend] is the combined char-color
    graphic with [char_name] from [rend.char_graphics] and a color grid
    of the matching size with only [color]. *)
val get_graphic_fill_color : string -> color -> t -> Raster.t

val get_layer : string -> t -> Raster.t

(** [init_rend layer_names size] is a Renderer with the layers of
    rasters necessary to run the game. Postcondition: Each raster in
    [(init_rend cells).layers] has the same dimensions. *)
val init_rend : string list -> point2d -> t

val render : t -> unit

val blank_layer : t -> Raster.t

val fill_layer : t -> char option -> color option -> Raster.t
