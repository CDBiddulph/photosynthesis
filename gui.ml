open Renderer
open Raster
open HexUtil

type t = {
  rend : Renderer.t;
  offsets : (string * point2d) list;
  player_params : (PlayerId.t * (char * ANSITerminal.color)) list;
  turn : PlayerId.t;
  store_costs : int list list;
  store_num_bought : int list;
}

let set_layer layer_name new_layer gui =
  { gui with rend = set_layer layer_name new_layer gui.rend }

let get_offset name gui = List.assoc name gui.offsets

let point2d_of_hex_coord gui coord =
  { x = coord.col * 11; y = (coord.diag * 6) - (coord.col * 3) + 9 }
  +: get_offset "board" gui

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
    |> ANSITerminal.(fill_color_in_raster White)
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

let plant_graphic plant gui =
  let char_name =
    "plants/" ^ Plant.(plant |> plant_stage |> string_of_plant_stage)
  in
  let color_name =
    match Plant.plant_stage plant with
    | Seed -> "plants/seed"
    | Small | Medium | Large -> "plants/tree"
  in
  let player_id = Plant.player_id plant in
  gui.rend
  |> get_graphic char_name color_name
  |> replace_char_in_raster 'x' (render_char player_id gui)
  |> replace_color_in_raster ANSITerminal.Default
       (render_color player_id gui)

(** [draw_plant layer_name plant point gui] returns [gui] with [plant]
    drawn in the position corresponding to [point] in the layer of
    [layer_name] according to the offset and graphics in [gui]. *)
let draw_plant layer_name plant point gui =
  let graphic = plant_graphic plant gui in
  draw_at_point layer_name graphic gui point

let draw_cells layer_name cells gui =
  let draw_cell gui cell =
    let point = cell |> Cell.coord |> point2d_of_hex_coord gui in
    match Cell.plant cell with
    | None -> draw_soil layer_name (Cell.soil cell) point gui
    | Some plant -> draw_plant layer_name plant point gui
  in
  List.fold_left draw_cell gui cells

let update_cells = draw_cells "cells"

let set_blank layer_name gui =
  set_layer layer_name (blank_layer gui.rend) gui

let draw_cursor layer_name color coord_opt gui =
  match coord_opt with
  | None -> gui
  | Some point ->
      let graphic =
        gui.rend |> get_graphic "hex" "hex"
        |> fill_color_in_raster color
      in
      draw_at_point layer_name graphic gui
        (point2d_of_hex_coord gui point)

let update_cursor color coord_opt gui =
  let layer_name = "cursor" in
  gui |> set_blank layer_name |> draw_cursor layer_name color coord_opt

let draw_text layer_name point text color gui =
  let text_graphic = text_raster text color in
  draw_at_point layer_name text_graphic gui point

let rec draw_text_lines layer_name point lines color gui =
  match lines with
  | [] -> gui
  | h :: t ->
      gui
      |> draw_text layer_name point h color
      |> draw_text_lines layer_name (point +: { x = 0; y = 1 }) t color

let update_message text color gui =
  gui |> set_blank "message"
  |> draw_text "message" { x = 0; y = 0 } text color

let update_sun dir gui = gui

let draw_plant_num layer_name point color num gui =
  gui
  |> draw_text layer_name
       (point +: get_offset "plant_num" gui)
       (string_of_int num) color

let plant_inv_point capacity origin row_i col_i =
  let max_capacity = 4 in
  let x = 8 * (max_capacity - capacity + col_i) in
  (* Equivalent to taking the sum from 4 to row_i + 4 *)
  let y = ((row_i * row_i) + (7 * row_i)) / 2 in
  origin +: { x; y }

let draw_row layer_name origin nums gui (row_i, graphic) =
  let capacity = List.nth nums row_i in
  List.fold_left
    (fun g col_i ->
      let top_left = plant_inv_point capacity origin row_i col_i in
      draw_at_point layer_name graphic g top_left)
    gui
    (List.rev (List.init capacity Fun.id))

let draw_plant_inventory layer_name offset nums gui =
  let enumerate_graphics =
    List.mapi
      (fun i s -> (i, plant_graphic (Plant.init_plant gui.turn s) gui))
      Plant.all_stages
  in
  List.fold_left
    (draw_row layer_name offset nums)
    gui
    (List.rev enumerate_graphics)

let draw_costs layer_name offset color limit_opt gui =
  let indexed_costs =
    List.mapi
      (fun row_i cost_row ->
        List.mapi (fun col_i cost -> (row_i, col_i, cost)) cost_row)
      gui.store_costs
    |> List.flatten
    |>
    match limit_opt with
    | None -> Fun.id
    | Some limits ->
        List.filter (fun (row_i, col_i, _) ->
            List.nth limits row_i > col_i)
  in
  List.fold_left
    (fun g (row_i, col_i, cost) ->
      let point =
        plant_inv_point
          (List.nth gui.store_costs row_i |> List.length)
          offset row_i col_i
      in
      draw_plant_num layer_name point color cost g)
    gui indexed_costs

let draw_bought layer_name offset color num_bought gui =
  gui |> set_blank layer_name
  |> draw_costs layer_name offset color (Some num_bought)

let update_bought num_bought gui =
  (* draw_plant_num cost_layer_name top_left color (List.nth cost col_i)
     g *)
  gui
  |> draw_bought "store_bought"
       (get_offset "store" gui)
       ANSITerminal.Magenta num_bought

let update_available num_available gui =
  gui
  |> draw_plant_inventory "available"
       (get_offset "available" gui)
       num_available

let draw_static_text layer_name gui =
  let draw_plant_inventory_static_text offset_name title =
    draw_text_lines layer_name
      (get_offset offset_name gui +: { x = 2; y = 1 })
      [ title; "-----------------------------" ]
      ANSITerminal.White
  in
  gui
  |> draw_plant_inventory_static_text "store" "Store"
  |> draw_plant_inventory_static_text "available" "Available"

let update_plant_highlight loc_opt gui = failwith "Unimplemented"

let update_turn player_id num_bought num_available highlight_loc_opt gui
    =
  let capacities = List.map List.length gui.store_costs in
  let store_offset = get_offset "store" gui in
  let new_turn_gui = { gui with turn = player_id } in
  new_turn_gui
  |> draw_plant_inventory "store_plants" store_offset capacities
  |> draw_costs "store_plants" store_offset
       (render_color new_turn_gui.turn new_turn_gui)
       None
  |> update_bought num_bought
  |> update_available num_available
(* |> update_plant_highlight highlight_loc_opt gui *)

let init_gui store_costs cells player_params =
  let layer_names =
    [
      "background";
      "hexes";
      "cursor";
      "cells";
      "store_plants";
      "store_bought";
      "available";
      "static text";
      "message";
    ]
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
      "plants/seed";
      "plants/tree";
      "soil/";
    ]
  in
  let gui =
    {
      rend =
        init_rend { x = 140; y = 45 } layer_names char_grid_names
          color_grid_names;
      offsets =
        [
          ("board", { x = 5; y = 2 });
          ("store", { x = 85; y = 0 });
          ("available", { x = 85; y = 23 });
          ("plant_num", { x = 5; y = 5 });
        ];
      player_params;
      turn = PlayerId.first;
      store_costs;
      store_num_bought = List.map (fun _ -> 0) Plant.all_stages;
    }
  in
  gui
  |> draw_hexes "hexes"
       (List.map
          (fun c -> c |> Cell.coord |> point2d_of_hex_coord gui)
          cells)
  |> draw_cells "cells" cells
  |> draw_static_text "static text"

let render gui = render gui.rend
