type point2d = {
  x : int;
  y : int;
}

type 'a grid = 'a option list list

type t = {
  char_grid : char grid;
  color_grid : ANSITerminal.color grid;
}

(** [p1 + p2] is the result of elementwise addition of [p1] and [p2]. *)
let ( +: ) p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }

(** [map_grid f grid] is a list of lists [result] with the dimensions of
    [grid1], where [result.(i).(j) = f grid1.(i).(j)]. *)
let map_grid f grid = List.map (fun row -> List.map f row) grid

(** [map2_grid f grid1 grid2] is a list of lists [result] with the
    dimensions of [grid1] and [grid2], where
    [result.(i).(j) = f grid1.(i).(j) grid2.(i).(j)]. Requires: the
    dimensions of grid1 are the same as the dimensions of grid2. *)
let map2_grid f grid1 grid2 =
  List.map2 (fun row1 row2 -> List.map2 f row1 row2) grid1 grid2

(** [map_offset f big_lst small_lst offset] is [big_lst] where all of
    the values stay the same, except for indices [offset] up to (not
    including) [offset + List.length small_lst]. For index [i] in
    small_lst, value [i + offset] in the new list is
    [f big_lst.(i + offset) small_lst.(i)]. Requires [offset >= 0] and
    [offset + List.length small_lst <= List.length big_lst]. *)
let map_offset f big_lst small_lst offset =
  assert (offset >= 0);
  let rec map_offset_helper f big_lst small_lst offset acc =
    match small_lst with
    | [] -> List.rev acc @ big_lst
    | sl_h :: sl_t -> (
        match offset with
        | 0 -> (
            match big_lst with
            | [] ->
                failwith
                  "offset + length small_lst exceeds length big_lst"
            | bl_h :: bl_t ->
                map_offset_helper f bl_t sl_t 0 (f bl_h sl_h :: acc))
        | o -> (
            (* print_int o; print_newline (); *)
            match big_lst with
            | [] -> failwith "offset exceeds length big_lst"
            | bl_h :: bl_t ->
                map_offset_helper f bl_t small_lst (o - 1) (bl_h :: acc)
            ))
  in
  map_offset_helper f big_lst small_lst offset []

let double_ended_opt f = function None -> None | Some x -> Some (f x)

let color_of_char ch =
  ANSITerminal.(
    match ch with
    | 'r' -> Red
    | 'g' -> Green
    | 'y' -> Yellow
    | 'b' -> Blue
    | 'm' -> Magenta
    | 'c' -> Cyan
    | 'w' -> White
    | 'k' -> Black
    | 'd' -> Default
    | other -> failwith ("invalid color code " ^ String.make 1 other))

let string_of_color color =
  ANSITerminal.(
    match color with
    | Red -> "red"
    | Green -> "green"
    | Yellow -> "yellow"
    | Blue -> "blue"
    | Magenta -> "magenta"
    | Cyan -> "cyan"
    | White -> "white"
    | Black -> "black"
    | Default -> "default")

let char_of_color ch =
  ANSITerminal.(
    match ch with Black -> 'k' | other -> (string_of_color other).[0])

let draw top_left graphic layer =
  let row_draw layer_r graphic_r =
    map_offset
      (* At least for now, ignore the layer beneath completely *)
        (fun layer_char graphic_char -> graphic_char)
      layer_r graphic_r top_left.x
  in
  {
    char_grid =
      map_offset row_draw layer.char_grid graphic.char_grid top_left.y;
    color_grid =
      map_offset row_draw layer.color_grid graphic.color_grid top_left.y;
  }

let rec fill_grid elem size =
  let rec fill_row elem w =
    match w with 0 -> [] | n -> elem :: fill_row elem (w - 1)
  in
  match size.y with
  | 0 -> []
  | n ->
      fill_row elem size.x :: fill_grid elem (size +: { x = 0; y = -1 })

let fill_raster size ch col =
  { char_grid = fill_grid ch size; color_grid = fill_grid col size }

(** [blank_raster w h] is a raster with a rectangular grid of [None]
    char options with width [w] and height [h]. *)
let blank_raster size = fill_raster size None None

let text_raster text color =
  let len = String.length text in
  {
    char_grid = [ List.init len (fun i -> Some (String.get text i)) ];
    color_grid = fill_grid (Some color) { x = len; y = 1 };
  }

(** [load_char_grid none_c filename] is a [char grid] representing the
    file at [filename] relative to the working directory. Each line
    ([char option list]) in the [grid] will contain a list of
    [char option]s up to, but not including a ['\n'] or end of file. The
    character [none_c] will be represented as [None], while all other
    characters [c] will be represented as [Some c]. It is possible for
    the grid to be represented by non-rectangular lists. *)
let load_char_grid none_c filename =
  let rec load_char_grid_helper ic g_acc =
    (* [load_line ic l_acc] is None when [ic] is at EOF. Otherwise, it
       is [Some line], where [line] is a char option list. [line] will
       contain char options up to, but not including the next ['\n'].
       The character [' '] will be represented as [None], while all
       other characters [c] will be represented as [Some c]. *)
    let rec load_line ic l_acc =
      try
        match input_char ic with
        | '\n' -> (
            match l_acc with
            | None -> Some []
            | Some a -> Some (List.rev a))
        | c ->
            let c_opt = if c = none_c then None else Some c in
            load_line ic
              (match l_acc with
              | None -> Some [ c_opt ]
              | Some a -> Some (c_opt :: a))
      with End_of_file -> (
        match l_acc with None -> None | Some a -> Some (List.rev a))
    in
    match load_line ic None with
    | None -> List.rev g_acc
    | Some line -> load_char_grid_helper ic (line :: g_acc)
  in
  let input_channel = open_in filename in
  let result = load_char_grid_helper input_channel [] in
  close_in input_channel;
  result

let load_char_grids none_c names =
  List.map
    (fun n -> (n, load_char_grid none_c ("graphics/" ^ n ^ ".txt")))
    names

let load_color_grids none_c names =
  List.map
    (fun n ->
      ( n,
        "graphics/" ^ n ^ ".color"
        |> load_char_grid none_c
        |> map_grid (double_ended_opt color_of_char) ))
    names

(** [replace_case_sens find_char replace_char input_char_opt] is [None]
    if [input_char_opt = None]. If [input_char_opt = Some input_char],
    returns [Some output_char], where [output_char] is [replace_char] if
    [input_char = find_char], the uppercase of [replace_char] if
    [input_char] equals the uppercase of [find_char], and [input_char]
    otherwise. Requires: [find_char] and [replace_char] are lowercase.*)
let replace_case_sens find_char replace_char input_char =
  if input_char = find_char then replace_char
  else if input_char = Char.uppercase_ascii find_char then
    Char.uppercase_ascii replace_char
  else input_char

let replace find_e replace_e input_e =
  if input_e = find_e then replace_e else input_e

let replace_color_in_raster find_color replace_color raster =
  {
    raster with
    color_grid =
      map_grid
        (double_ended_opt (replace find_color replace_color))
        raster.color_grid;
  }

let replace_char_in_raster find_char replace_char raster =
  {
    raster with
    char_grid =
      map_grid
        (double_ended_opt (replace_case_sens find_char replace_char))
        raster.char_grid;
  }

let merge_two_rasters under over =
  let filter_color ch_opt col_opt =
    if ch_opt = None then None else col_opt
  in
  let over_filtered_colors =
    map2_grid filter_color over.char_grid over.color_grid
  in
  let merge_two u o = match o with None -> u | o -> o in
  {
    char_grid = map2_grid merge_two under.char_grid over.char_grid;
    color_grid =
      map2_grid merge_two under.color_grid over_filtered_colors;
  }

let merge_rasters raster_order rasters =
  let rec merge_rasters_helper raster_order rasters acc =
    match raster_order with
    | [] -> acc
    | h :: t ->
        merge_rasters_helper t rasters
          (merge_two_rasters acc (List.assoc h rasters))
  in
  match raster_order with
  | [] -> blank_raster { x = 0; y = 0 }
  | [ h ] -> List.assoc h rasters
  | h :: t -> merge_rasters_helper t rasters (List.assoc h rasters)
