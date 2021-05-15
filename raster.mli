(** Contains operations that can be performed on two equally-sized
    rectangular grids of (optional) characters and colors, collectively
    called a raster. *)

(** [point2d] contains an x and a y coordinate. Meant to correspond to
    positions that characters can occupy on the screen. *)
type point2d = {
  x : int;
  y : int;
}

(** A grid is a list of lists of options, where each sub-list is a row. *)
type 'a grid = 'a option list list

(** A Raster.t consists of a character grid and a color grid.
    Corresponding positions in the two grids store the character that
    should be rendered to the screen at that position and its color. *)
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

(** [map_grid f grid] is a list of lists [result] with the dimensions of
    [grid1], where [result.(i).(j) = f grid1.(i).(j)]. *)
val map_grid : ('a option -> 'b option) -> 'a grid -> 'b grid

(** [map2_grid f grid1 grid2] is a list of lists [result] with the
    dimensions of [grid1] and [grid2], where
    [result.(i).(j) = f grid1.(i).(j) grid2.(i).(j)]. Requires: the
    dimensions of grid1 are the same as the dimensions of grid2. *)
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

(** [blank_raster size ch col] is a raster with a rectangular grid
    filled with [ch] and [col] with width [size.x] and height [size.y]. *)
val fill_raster :
  point2d -> char option -> ANSITerminal.color option -> t

(** [blank_raster size] is a raster with a rectangular grid of [None]
    char options with width [size.x] and height [size.y]. *)
val blank_raster : point2d -> t

(** [text_raster text color] is a raster that contains just one line,
    with the text of [text] and color [color]. *)
val text_raster : string -> ANSITerminal.color -> t

(** [load_char_grids none_c filenames] is a [char grid lst] representing
    the file at "graphics/{filename}.txt" relative to the working
    directory. Each line ([char option list]) in each [grid] will
    contain a list of [char option]s up to, but not including a ['\n']
    or end of file. The character [none_c] will be represented as
    [None], while all other characters [c] will be represented as [Some
    c]. It is possible for the grids to be represented by
    non-rectangular lists. *)
val load_char_grids : char -> string list -> (string * char grid) list

(** [load_color_grids none_c filenames] is a [ANSITerminal.color grid
    lst] representing the file at "graphics/{filename}.color" relative
    to the working directory. Each line ([ANSITerminal.color option
    list]) in each [grid] will contain a list of [ANSITerminal.color
    option]s up to, but not including a ['\n'] or end of file. The
    character [none_c] will be represented as [None], while all other
    characters [c] will be represented as [Some c]. It is possible for
    the grids to be represented by non-rectangular lists. *)
val load_color_grids :
  char -> string list -> (string * ANSITerminal.color grid) list

(** [merge_rasters raster_order rasters] is a raster formed by merging
    each raster in [rasters] from back to front in the order obtained by
    looking up each of them in the order of [raster_order]. If a raster
    name is in [raster_order] but does not correspond to a raster in
    [rasters]. it is simply skipped. When a [None] character is merged
    over another character, the None character is ignored and the
    character underneath is unchanged. Even if a color is not [None], if
    its corresponding character is [None], it will be ignored.
    Precondition: all rasters have the same dimensions. *)
val merge_rasters : string list -> (string * t) list -> t
