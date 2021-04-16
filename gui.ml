open Renderer
open Raster
open HexUtil

type t = {
  rend : Renderer.t;
  hex_offset : point2d;
  board_offset : point2d;
  player_params : (PlayerId.t * (char * ANSITerminal.color)) list;
}

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

(** [draw_hexes gui coords layer] returns [layer] with hexes drawn on it
    in the positions corresponding to [coords] with an offset of
    [gui.hex_offset]. *)
let draw_hexes gui coords layer =
  let hex_graphic =
    gui.rend |> get_graphic "hex" "hex"
    |> ANSITerminal.(replace_color_in_raster Default White)
  in
  List.fold_left (draw_in_coord gui hex_graphic) layer coords

(** [draw_soil gui coord soil layer] returns [layer] with a soil marker
    for [soil] drawn on it in the position corresponding to [coord] with
    an offset of [gui.hex_offset]. *)
let draw_soil gui coord soil layer =
  let char_grid_name = "soil/" ^ string_of_int soil in
  let soil_graphic = get_graphic char_grid_name "soil/" gui.rend in
  draw_in_coord gui soil_graphic layer coord

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
    gui.rend
    |> get_graphic char_name color_name
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

let draw_text text color point gui = draw (*TODO*)

(** [update_cells cells gui] is [gui] with the contents of each cell in
    [cells] updated. If [Cell.plant c = None] for some [c] in [cells],
    the space corresponding to [c] will display a marker showing the
    type of soil in [c]. Otherwise, if [Cell.plant c = Some p], [p] will
    be displayed. *)
let update_cells cells gui =
  {
    gui with
    rend = gui.rend |> apply_to_layer "cells" (draw_cells gui cells);
  }

let draw_cursor gui color coord_opt layer =
  let blank = blank_layer gui.rend in
  match coord_opt with
  | None -> blank
  | Some coord ->
      let graphic =
        gui.rend |> get_graphic "hex" "hex"
        |> replace_color_in_raster ANSITerminal.Default color
      in
      draw_in_coord gui graphic blank coord

let update_cursor color coord_opt gui =
  {
    gui with
    rend =
      gui.rend
      |> apply_to_layer "cursor" (draw_cursor gui color coord_opt);
  }

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
  let gui =
    {
      rend =
        init_rend { x = 120; y = 45 } layer_names char_grid_names
          color_grid_names;
      hex_offset = { x = 0; y = 9 };
      board_offset = { x = 5; y = 2 };
      player_params;
    }
  in
  {
    gui with
    rend =
      gui.rend
      |> apply_to_layer "hexes"
           (draw_hexes gui (List.map Cell.coord cells));
  }
  |> update_cells cells

let render gui = render gui.rend
