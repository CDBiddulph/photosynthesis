open HexUtil
open Raster
open ANSITerminal

type t

(** [set_layer layer_name new_layer rend] is [rend] with the layer with
    [layer_name] set to [new_layer]. Precondition: a layer with
    [layer_name] must exist in [rend]. *)
val set_layer : string -> Raster.t -> t -> t

(** [get_graphic char_name color_name rend] is the combined char-color
    graphic with [char_name] from [rend.char_graphics] and [color_name]
    from [rend.color_graphics]. *)
val get_graphic_with_color_grid : string -> string -> t -> Raster.t

(** [get_graphic char_name color rend] is the combined char-color
    graphic with [char_name] from [rend.char_graphics] and a color grid
    of the matching size with only [color]. *)
val get_graphic_fill_color : string -> color -> t -> Raster.t

(** [get_layer name rend] is the layer with [name] in [rend]. *)
val get_layer : string -> t -> Raster.t

(** [set_visible visible layer_name rend] is [rend] with the layer with
    [layer_name] set as visible if [visible], otherwise not visible. *)
val set_visible : bool -> string -> t -> t

(** [init_rend layer_info size] is a Renderer with its layers derived
    from [layer_info], where the first entry in each pair is the layer's
    name and the second its visibility. The layers of [layer_info] are
    ordered from back to front. All layers are set to blank rasters of
    [size]. *)
val init_rend : (string * bool) list -> point2d -> t

(** [render rend] renders all visible layers in [rend] to the screen, in
    order. *)
val render : t -> unit

(** [blank_layer rend] is a blank layer that fits the size of the other
    layers in [rend]. *)
val blank_layer : t -> Raster.t

(** [fill_layer rend char_opt col_opt] is a layer that just repeats
    [char_opt] and [color_opt], and fits the size of the other layers in
    [rend]. *)
val fill_layer : t -> char option -> color option -> Raster.t
