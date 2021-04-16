open HexUtil
open Raster

type t = {
  width : int;
  height : int;
  layers : (string * Raster.t) list;
  layer_order : string list;
  char_graphics : (string * char grid) list;
  color_graphics : (string * ANSITerminal.color grid) list;
  hex_offset : point2d;
  board_offset : point2d;
  player_params : (PlayerId.t * (char * ANSITerminal.color)) list;
}

let apply_to_layer layer_name f gui =
  let new_layer = f (List.assoc layer_name gui.layers) in
  {
    gui with
    layers =
      (layer_name, new_layer) :: List.remove_assoc layer_name gui.layers;
  }

(* Could be used for testing later *)
(* gui |> update_layer "hexes" (draw gui "hex" 0 0) |> update_layer
   "hexes2" (draw gui "hex" 1 1) |> update_layer "hexes" (draw gui
   "empty" 10 5) |> update_layer "hexes2" (draw gui "horiz" 80 29) |>
   update_layer "hexes" (draw gui "vert" 99 0) |> update_layer "hexes"
   (draw gui "hex" 91 25) |> update_layer "background" (draw gui "hex"
   50 5) *)

let update_sun dir gui = gui

let point2d_of_hex_coord gui coord =
  { x = coord.col * 11; y = (coord.diag * 6) - (coord.col * 3) }
  +: gui.hex_offset

(** [draw_in_coord gui graphic_name layer coord] returns [layer] with
    the graphic with name [graphic_name] of [gui] drawn in the position
    corresponding to [coord] with an offset of [gui.hex_offset]. *)
let draw_in_coord gui graphic layer coord =
  let top_left = point2d_of_hex_coord gui coord in
  draw graphic layer (top_left +: gui.board_offset)

(** [get_graphic gui char_name color_name] is the combined char-color
    graphic with [char_name] from [gui.char_graphics] and [color_name]
    from [gui.color_graphics]. *)
let get_graphic gui char_name color_name =
  {
    char_grid = List.assoc char_name gui.char_graphics;
    color_grid = List.assoc color_name gui.color_graphics;
  }

(** [draw_hexes gui coords layer] returns [layer] with hexes drawn on it
    in the positions corresponding to [coords] with an offset of
    [gui.hex_offset]. *)
let draw_hexes gui coords layer =
  List.fold_left
    (draw_in_coord gui
       (get_graphic gui "hex" "hex"
       |> ANSITerminal.(replace_color_in_raster Default White)))
    layer coords

(** [draw_soil gui coord soil layer] returns [layer] with a soil marker
    for [soil] drawn on it in the position corresponding to [coord] with
    an offset of [gui.hex_offset]. *)
let draw_soil gui coord soil layer =
  let char_name = "soil/" ^ string_of_int soil in
  draw_in_coord gui (get_graphic gui char_name "soil/") layer coord

let render_char player_id gui =
  fst (List.assoc player_id gui.player_params)

let render_color player_id gui =
  snd (List.assoc player_id gui.player_params)

(** [draw_plant gui coord plant layer] returns [layer] with [plant]
    drawn in the position corresponding to [coord] according to the
    hex_offset and graphics in [gui]. *)
let draw_plant gui coord plant layer =
  let char_name =
    "plants/" ^ Plant.(plant |> plant_stage |> string_of_plant_stage)
  in
  let color_name = "plants/tree" in
  let graphic =
    let player_id = Plant.player_id plant in
    get_graphic gui char_name color_name
    |> replace_char_in_raster 'x' (render_char player_id gui)
    |> replace_color_in_raster ANSITerminal.Default
         (render_color player_id gui)
  in
  draw_in_coord gui graphic layer coord

(** [draw_cells gui cells layer] returns [layer] with the contents of
    [cells] (either soil or a plant, depending on whether the cell has a
    plant or not) in the corresponding positions, with the graphics and
    offset in [gui] *)
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

let draw_cursor gui color coord_opt layer =
  let blank = blank_raster gui.width gui.height in
  match coord_opt with
  | None -> blank
  | Some coord ->
      let graphic =
        get_graphic gui "hex" "hex"
        |> replace_color_in_raster ANSITerminal.Default color
      in
      draw_in_coord gui graphic blank coord

let update_cursor color coord_opt gui =
  gui |> apply_to_layer "cursor" (draw_cursor gui color coord_opt)

(** [init_gui cells] is a GUI with the layers of rasters necessary to
    run the game. Postcondition: Each raster in
    [(init_gui cells).layers] has the same dimensions. *)
let init_gui cells player_params =
  let w = 120 in
  let h = 50 in
  let background =
    fill_raster (Some '.') (Some ANSITerminal.Magenta) w h
  in
  (* layers must be in order from back to front, since it will be used
     to make layer_order *)
  let layers =
    [
      ("background", background);
      ("hexes", blank_raster w h);
      ("cursor", blank_raster w h);
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
            "plants/seed";
            "plants/small";
            "plants/medium";
            "plants/large";
            "soil/1";
            "soil/2";
            "soil/3";
            "soil/4";
          ];
      color_graphics =
        load_color_grids '`'
          [
            "hex";
            "miscellaneous/dot";
            "miscellaneous/empty";
            "miscellaneous/vert";
            "miscellaneous/horiz";
            "plants/tree";
            "soil/";
          ];
      hex_offset = { x = 0; y = 9 };
      board_offset = { x = 10; y = 5 };
      player_params;
    }
  in
  gui
  |> apply_to_layer "hexes" (draw_hexes gui (List.map Cell.coord cells))
  |> update_cells cells

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
  let render_raster = merge_rasters gui.layer_order gui.layers in
  ignore (Sys.command "clear");
  print_raster render_raster
