open HexUtil

type point2d = {
  x : int;
  y : int;
}

type 'a grid = 'a option list list

type raster = {
  char_grid : char grid;
  color_grid : ANSITerminal.color grid;
}

type t = {
  width : int;
  height : int;
  layers : (string * raster) list;
  layer_order : string list;
  char_graphics : (string * char grid) list;
  color_graphics : (string * ANSITerminal.color grid) list;
  hex_offset : point2d;
}

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
    [f big_lst.(i + offset) small_lst.(i)]. *)
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
                map_offset_helper f bl_t sl_t 0 (f bl_h sl_h :: acc) )
        | o -> (
            (* print_int o; print_newline (); *)
            match big_lst with
            | [] -> failwith "offset exceeds length big_lst"
            | bl_h :: bl_t ->
                map_offset_helper f bl_t small_lst (o - 1) (bl_h :: acc)
            ) )
  in
  map_offset_helper f big_lst small_lst offset []

let color_opt_of_char_opt c_opt =
  match c_opt with
  | None -> None
  | Some c ->
      Some
        ANSITerminal.(
          match c with
          | 'r' -> Red
          | 'g' -> Green
          | 'y' -> Yellow
          | 'b' -> Blue
          | 'm' -> Magenta
          | 'c' -> Cyan
          | 'w' -> White
          | 'k' -> Black
          | 'd' -> Default
          | other ->
              failwith ("invalid color code " ^ String.make 1 other))

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

let draw gui char_name color_name layer top_left =
  let char_graphic = List.assoc char_name gui.char_graphics in
  let color_graphic = List.assoc color_name gui.color_graphics in
  let row_draw layer_r graphic_r =
    map_offset
      (fun layer_char graphic_char ->
        match graphic_char with None -> layer_char | c -> c)
      layer_r graphic_r top_left.x
  in
  {
    char_grid =
      map_offset row_draw layer.char_grid char_graphic top_left.y;
    color_grid =
      map_offset row_draw layer.color_grid color_graphic top_left.y;
  }

let apply_to_layer layer_name f gui =
  let new_layer = f (List.assoc layer_name gui.layers) in
  {
    gui with
    layers =
      (layer_name, new_layer) :: List.remove_assoc layer_name gui.layers;
  }

(* gui |> update_layer "hexes" (draw gui "hex" 0 0) |> update_layer
   "hexes2" (draw gui "hex" 1 1) |> update_layer "hexes" (draw gui
   "empty" 10 5) |> update_layer "hexes2" (draw gui "horiz" 80 29) |>
   update_layer "hexes" (draw gui "vert" 99 0) |> update_layer "hexes"
   (draw gui "hex" 91 25) |> update_layer "background" (draw gui "hex"
   50 5) *)

let update_sun gui dir = gui

let fill_raster ch col w h =
  let rec fill_grid elem w h =
    let rec fill_row elem w =
      match w with 0 -> [] | n -> elem :: fill_row elem (w - 1)
    in
    match h with
    | 0 -> []
    | n -> fill_row elem w :: fill_grid elem w (h - 1)
  in
  { char_grid = fill_grid ch w h; color_grid = fill_grid col w h }

(** [blank_raster w h] is a raster with a rectangular grid of [None]
    char options with width [w] and height [h]. *)
let blank_raster w h = fill_raster None None w h

(** [null_raster] is a raster with an empty grid and 0 width and height. *)
let null_raster = blank_raster 0 0

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
            | Some a -> Some (List.rev a) )
        | c ->
            let c_opt = if c = none_c then None else Some c in
            load_line ic
              ( match l_acc with
              | None -> Some [ c_opt ]
              | Some a -> Some (c_opt :: a) )
      with End_of_file -> (
        match l_acc with None -> None | Some a -> Some (List.rev a) )
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
        |> map_grid color_opt_of_char_opt ))
    names

let point2d_of_hex_coord gui coord =
  let x = coord.col * 11 in
  let y = (coord.diag * 6) - (coord.col * 3) in
  { x = x + gui.hex_offset.x; y = y + gui.hex_offset.y }

(** [draw_in_coord gui graphic_name layer coord] returns [layer] with
    the graphic with name [graphic_name] of [gui] drawn in the position
    corresponding to [coord] with an offset of [gui.hex_offset]. *)
let draw_in_coord gui char_name color_name layer coord =
  let top_left = point2d_of_hex_coord gui coord in
  draw gui char_name color_name layer top_left

(** [draw_hexes gui coords layer] returns [layer] with hexes drawn on it
    in the positions corresponding to [coords] with an offset of
    [gui.hex_offset]. *)
let draw_hexes gui coords layer =
  List.fold_left (draw_in_coord gui "hex" "hex") layer coords

(** [draw_soil gui coord soil layer] returns [layer] with a soil marker
    for [soil] drawn on it in the position corresponding to [coord] with
    an offset of [gui.hex_offset]. *)
let draw_soil gui coord soil layer =
  let name = "soil" ^ string_of_int soil in
  draw_in_coord gui name name layer coord

(** [draw_hexes gui cells] returns [gui] with hexes drawn in its "hexes"
    layer with the positions corresponding to the coords of each cell in
    [cells]. *)
let draw_plant gui coord plant layer =
  let char_name =
    "plants/"
    ^ Plant.(
        (plant |> plant_stage |> Plant.string_of_plant_stage)
        ^ "_"
        ^ String.make 1 (Plant.render_char plant))
  in
  let color_name =
    "plants/tree_" ^ string_of_color (Plant.render_color plant)
  in
  draw_in_coord gui char_name color_name layer coord

let draw_cells gui cells layer =
  let draw_cell gui layer cell =
    match Cell.plant cell with
    | None -> draw_soil gui (Cell.coord cell) (Cell.soil cell) layer
    | Some p -> draw_plant gui (Cell.coord cell) p layer
  in
  List.fold_left (draw_cell gui) layer cells

(* [update_cells cells gui] is [gui] with the contents of each cell in
   [cells] updated. If [Cell.plant c = None] for some [c] in [cells],
   the space corresponding to [c] will display a marker showing the type
   of soil in [c]. Otherwise, if [Cell.plant c = Some p], [p] will be
   displayed. *)
let update_cells cells gui =
  gui |> apply_to_layer "cells" (draw_cells gui cells)

(* draw_in_cell gui "small_x_tree" *)

(** [init_gui cells] is a GUI with the layers of rasters necessary to
    run the game. Postcondition: Each raster in
    [(init_gui cells).layers] has the same dimensions. *)
let init_gui cells =
  let w = 76 in
  let h = 41 in
  let background =
    fill_raster (Some '.') (Some ANSITerminal.Magenta) w h
  in
  (* layers must be in order from back to front, since it will be used
     to make layer_order *)
  let layers =
    [
      ("background", background);
      ("hexes", blank_raster w h);
      ("cells", blank_raster w h);
    ]
  in
  let gui =
    {
      width = w;
      height = h;
      layers;
      layer_order = List.map fst layers;
      char_graphics =
        load_char_grids '`'
          [
            "hex";
            "miscellaneous/dot";
            "miscellaneous/empty";
            "miscellaneous/vert";
            "miscellaneous/horiz";
            "plants/seed_x";
            "plants/small_x";
            "plants/medium_x";
            "plants/large_x";
          ];
      color_graphics =
        load_color_grids '`'
          [
            "hex";
            "miscellaneous/dot";
            "miscellaneous/empty";
            "miscellaneous/vert";
            "miscellaneous/horiz";
            "plants/tree_cyan";
            "plants/tree_green";
            "plants/tree_red";
            "plants/tree_yellow";
          ];
      hex_offset = { x = 0; y = 9 };
    }
  in
  gui
  |> apply_to_layer "hexes" (draw_hexes gui (List.map Cell.coord cells))
  |> update_cells cells

let merge_two_layers under over =
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

let merge_layers layer_order layers =
  let rec merge_layers_helper layer_order layers acc =
    match layer_order with
    | [] -> acc
    | h :: t ->
        merge_layers_helper t layers
          (merge_two_layers acc (List.assoc h layers))
  in
  match layer_order with
  | [] -> null_raster
  | [ h ] -> List.assoc h layers
  | h :: t -> merge_layers_helper t layers (List.assoc h layers)

let render gui =
  let print_raster raster =
    let print_row char_row color_row =
      List.iter2
        (fun char_opt color_opt ->
          match (char_opt, color_opt) with
          | None, None -> print_char ' '
          | None, Some _ -> print_char ' '
          | Some ch, None -> print_char ch
          | Some ch, Some col ->
              ANSITerminal.print_string [ Foreground col ]
                (String.make 1 ch))
        char_row color_row
    in
    List.iter2
      (fun char_row color_row ->
        print_row char_row color_row;
        print_newline ())
      raster.char_grid raster.color_grid
  in
  let render_raster = merge_layers gui.layer_order gui.layers in
  ignore (Sys.command "clear");
  print_raster render_raster
