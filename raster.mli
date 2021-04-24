type point2d = {
  x : int;
  y : int;
}

type 'a grid = 'a option list list

type t = {
  char_grid : char grid;
  color_grid : ANSITerminal.color grid;
}

(** [p1 +: p2] is the result of elementwise addition of [p1] and [p2]. *)
val ( +: ) : point2d -> point2d -> point2d

(** [p1 *: p2] is the result of elementwise multiplication of [p1] and
    [p2]. *)
val ( *: ) : point2d -> point2d -> point2d

(** [draw top_left graphic layer] is [layer] with [graphic] superimposed
    on top of it, with the top-left corner of [graphic] at [top_left]. *)
val draw : point2d -> t -> t -> t

val map_grid : ('a option -> 'b option) -> 'a grid -> 'b grid

val map2_grid :
  ('a option -> 'b option -> 'c option) -> 'a grid -> 'b grid -> 'c grid

(** [replace_char find_char replace_char raster] is [raster] with all
    instances of [find_char] replaced by [replace_char], but
    case-sensitive, so all uppercase versions of [find_char] will be
    replaced with the uppercase version of [replace_char]. *)
val replace_char : char -> char -> t -> t

(** [replace_char_with_none find_char raster] is [raster] with all
    instances of [Some find_char] replaced by [None]. Not
    case-sensitive. *)
val replace_char_with_none : char -> t -> t

(** [replace_color find_color replace_color raster] is [raster] with all
    instances of [find_color] replaced by [replace_color]. *)
val replace_color : ANSITerminal.color -> ANSITerminal.color -> t -> t

(** [replace_all_color fill_color raster] is [raster] with all colors in
    the color grid of raster replaced by [fill_color]. *)
val replace_all_color : ANSITerminal.color -> t -> t

val fill_raster :
  point2d -> char option -> ANSITerminal.color option -> t

val blank_raster : point2d -> t

(** [text_raster text color] is a raster that contains just one line,
    with the text of [text] and color [color]. *)
val text_raster : string -> ANSITerminal.color -> t

val load_char_grids : char -> string list -> (string * char grid) list

val load_color_grids :
  char -> string list -> (string * ANSITerminal.color grid) list

val merge_rasters : string list -> (string * t) list -> t
