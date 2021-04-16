type point2d = {
  x : int;
  y : int;
}

type 'a grid = 'a option list list

type t = {
  char_grid : char grid;
  color_grid : ANSITerminal.color grid;
}

val ( +: ) : point2d -> point2d -> point2d

val draw : t -> t -> point2d -> t

val map_grid : ('a option -> 'b option) -> 'a grid -> 'b grid

val map2_grid :
  ('a option -> 'b option -> 'c option) -> 'a grid -> 'b grid -> 'c grid

val replace_char_in_raster : char -> char -> t -> t

val replace_color_in_raster :
  ANSITerminal.color -> ANSITerminal.color -> t -> t

val fill_raster :
  char option -> ANSITerminal.color option -> int -> int -> t

val blank_raster : int -> int -> t

val load_char_grids : char -> string list -> (string * char grid) list

val load_color_grids :
  char -> string list -> (string * ANSITerminal.color grid) list

val merge_rasters : string list -> (string * t) list -> t
