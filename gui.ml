open Renderer
open Raster
open HexUtil

type t = {
  rend : Renderer.t;
  board_offset : point2d;
  player_params : (PlayerId.t * (char * ANSITerminal.color)) list;
}

let set_layer layer_name new_layer gui =
  { gui with rend = set_layer layer_name new_layer gui.rend }

let point2d_of_hex_coord gui coord =
  { x = coord.col * 11; y = (coord.diag * 6) - (coord.col * 3) + 9 }
  +: gui.board_offset

(** [draw_at_point gui graphic_name layer point] returns [layer] with
    the graphic with name [graphic_name] of [gui] drawn in the position
    corresponding to [point] *)
let draw_at_point layer_name graphic gui point =
  let new_layer = draw point graphic (get_layer layer_name gui.rend) in
  set_layer layer_name new_layer gui

(** [draw_hexes gui points layer] returns [layer] with hexes drawn on it
    in the positions corresponding to [points] with an offset of
    [gui.hex_offset] + [gui.board_offset]. *)
let draw_hexes layer_name points gui =
  let hex_graphic =
    gui.rend |> get_graphic "hex" "hex"
    |> ANSITerminal.(replace_color_in_raster Default White)
  in
  List.fold_left (draw_at_point layer_name hex_graphic) gui points

(** [draw_soil gui point soil layer] returns [layer] with a soil marker
    for [soil] drawn on it in the position corresponding to [point] with
    an offset of [gui.hex_offset] + [gui.board_offset]. *)
let draw_soil layer_name soil point gui =
  let char_grid_name = "soil/" ^ string_of_int soil in
  let soil_graphic = get_graphic char_grid_name "soil/" gui.rend in
  draw_at_point layer_name soil_graphic gui point

let render_char player_id gui =
  fst (List.assoc player_id gui.player_params)

let render_color player_id gui =
  snd (List.assoc player_id gui.player_params)

(** [draw_plant gui point plant layer] returns [layer] with [plant]
    drawn in the position corresponding to [point] according to the
    offset and graphics in [gui]. *)
let draw_plant layer_name plant point gui =
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
  draw_at_point layer_name graphic gui point

let draw_cells layer_name cells gui =
  let draw_cell gui cell =
    let point = cell |> Cell.coord |> point2d_of_hex_coord gui in
    match Cell.plant cell with
    | None -> draw_soil layer_name (Cell.soil cell) point gui
    | Some plant -> draw_plant layer_name plant point gui
  in
  List.fold_left draw_cell gui cells

let set_blank layer_name gui =
  set_layer layer_name (blank_layer gui.rend) gui

let draw_cursor layer_name color point_opt gui =
  match point_opt with
  | None -> gui
  | Some point ->
      let graphic =
        gui.rend |> get_graphic "hex" "hex"
        |> replace_color_in_raster ANSITerminal.Default color
      in
      draw_at_point layer_name graphic gui
        (point2d_of_hex_coord gui point)

let draw_text point text color gui =
  let text_graphic = text_raster text color in
  draw_at_point "text" text_graphic gui point

let init_gui cells player_params =
  let layer_names =
    [ "background"; "hexes"; "cursor"; "cells"; "text" ]
  in
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
  let gui =
    {
      rend =
        init_rend { x = 120; y = 45 } layer_names char_grid_names
          color_grid_names;
      board_offset = { x = 5; y = 2 };
      player_params;
    }
  in
  gui
  |> draw_hexes "hexes"
       (List.map
          (fun c -> c |> Cell.coord |> point2d_of_hex_coord gui)
          cells)
  |> draw_cells "cells" cells

let update_cells = draw_cells "cells"

let update_sun dir gui = gui

let update_cursor color coord gui =
  let layer_name = "cursor" in
  gui |> set_blank layer_name |> draw_cursor layer_name color coord

let update_message text color gui =
  gui |> set_blank "text" |> draw_text { x = 0; y = 0 } text color

let render gui = render gui.rend
