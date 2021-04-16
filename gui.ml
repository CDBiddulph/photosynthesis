open Renderer
open Raster
open HexUtil

type t = {
  rend : Renderer.t;
  hex_offset : point2d;
  board_offset : point2d;
  player_params : (PlayerId.t * (char * ANSITerminal.color)) list;
}

let set_layer layer_name new_layer gui =
  { gui with rend = set_layer layer_name new_layer gui.rend }

let point2d_of_hex_coord gui coord =
  { x = coord.col * 11; y = (coord.diag * 6) - (coord.col * 3) }
  +: gui.hex_offset

(** [draw_in_coord gui graphic_name layer coord] returns [layer] with
    the graphic with name [graphic_name] of [gui] drawn in the position
    corresponding to [coord] with an offset of [gui.hex_offset]. *)
let draw_in_coord layer_name graphic gui coord =
  let top_left = point2d_of_hex_coord gui coord in
  let new_layer =
    draw
      (top_left +: gui.board_offset)
      graphic
      (get_layer layer_name gui.rend)
  in
  set_layer layer_name new_layer gui

(** [draw_hexes gui coords layer] returns [layer] with hexes drawn on it
    in the positions corresponding to [coords] with an offset of
    [gui.hex_offset]. *)
let draw_hexes layer_name coords gui =
  let hex_graphic =
    gui.rend |> get_graphic "hex" "hex"
    |> ANSITerminal.(replace_color_in_raster Default White)
  in
  List.fold_left (draw_in_coord layer_name hex_graphic) gui coords

(** [draw_soil gui coord soil layer] returns [layer] with a soil marker
    for [soil] drawn on it in the position corresponding to [coord] with
    an offset of [gui.hex_offset]. *)
let draw_soil layer_name soil coord gui =
  let char_grid_name = "soil/" ^ string_of_int soil in
  let soil_graphic = get_graphic char_grid_name "soil/" gui.rend in
  draw_in_coord layer_name soil_graphic gui coord

let render_char player_id gui =
  fst (List.assoc player_id gui.player_params)

let render_color player_id gui =
  snd (List.assoc player_id gui.player_params)

(** [draw_plant gui coord plant layer] returns [layer] with [plant]
    drawn in the position corresponding to [coord] according to the
    hex_offset and graphics in [gui]. *)
let draw_plant layer_name coord plant gui =
  let char_name =
    "plants/" ^ Plant.(plant |> plant_stage |> string_of_plant_stage)
  in
  let color_name = "plants/tree" in
  let graphic =
    let player_id = Plant.player_id plant in
    gui.rend
    |> get_graphic char_name color_name
    |> replace_char_in_raster 'x' (render_char player_id gui)
    |> replace_color_in_raster ANSITerminal.Default
         (render_color player_id gui)
  in
  draw_in_coord layer_name graphic gui coord

let draw_cells layer_name cells gui =
  let draw_cell gui cell =
    match Cell.plant cell with
    | None ->
        draw_soil layer_name (Cell.soil cell) (Cell.coord cell) gui
    | Some p -> draw_plant layer_name (Cell.coord cell) p gui
  in
  List.fold_left draw_cell gui cells

let set_blank layer_name gui =
  set_layer layer_name (blank_layer gui.rend) gui

let draw_cursor layer_name color coord_opt gui =
  match coord_opt with
  | None -> gui
  | Some coord ->
      let graphic =
        gui.rend |> get_graphic "hex" "hex"
        |> replace_color_in_raster ANSITerminal.Default color
      in
      draw_in_coord layer_name graphic gui coord

let draw_text text color point gui = draw (*TODO*)

let init_gui cells player_params =
  let layer_names = [ "background"; "hexes"; "cursor"; "cells" ] in
  let char_grid_names =
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
    ]
  in
  let color_grid_names =
    [
      "hex";
      "miscellaneous/dot";
      "miscellaneous/empty";
      "miscellaneous/vert";
      "miscellaneous/horiz";
      "plants/tree";
      "soil/";
    ]
  in
  {
    rend =
      init_rend { x = 120; y = 45 } layer_names char_grid_names
        color_grid_names;
    hex_offset = { x = 0; y = 9 };
    board_offset = { x = 5; y = 2 };
    player_params;
  }
  |> draw_hexes "hexes" (List.map Cell.coord cells)
  |> draw_cells "cells" cells

let update_cells = draw_cells "cells"

let update_sun dir gui = gui

let update_cursor color coord gui =
  let layer_name = "cursor" in
  gui |> set_blank layer_name |> draw_cursor layer_name color coord

let render gui = render gui.rend
