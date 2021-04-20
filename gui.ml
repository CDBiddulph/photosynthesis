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

(** [draw_plant layer_name plant point gui] returns [gui] with [plant]
    drawn in the position corresponding to [point] in the layer of
    [layer_name] according to the offset and graphics in [gui]. *)
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

let update_cells = draw_cells "cells"

let set_blank layer_name gui =
  set_layer layer_name (blank_layer gui.rend) gui

let draw_cursor layer_name color coord_opt gui =
  match coord_opt with
  | None -> gui
  | Some point ->
      let graphic =
        gui.rend |> get_graphic "hex" "hex"
        |> replace_color_in_raster ANSITerminal.Default color
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

let update_turn player_id gui = { gui with turn = player_id }

let draw_plant_num layer_name point color num gui =
  gui
  |> draw_text layer_name
       (point +: get_offset "plant_num" gui)
       (string_of_int num) color

let plant_inv_point max_capacity origin row_i col_i =
  let x = 8 * (max_capacity - 1 - col_i) in
  (* Equivalent to taking the sum from 4 to row_i + 4 *)
  let y = ((row_i * row_i) + (7 * row_i)) / 2 in
  origin +: { x; y }

let draw_plant_row
    plant_layer_name
    cost_layer_name
    player_id
    origin
    nums
    costs_opt
    gui
    (row_i, stage) =
  let plant = Plant.init_plant player_id stage in
  let capacity = List.nth nums row_i in
  let row_costs_opt =
    match costs_opt with
    | None -> None
    | Some costs -> Some (List.nth costs row_i)
  in
  let color = render_color player_id gui in
  let num_draw_f top_left col_i g =
    match row_costs_opt with
    | None -> g
    | Some cost ->
        draw_plant_num cost_layer_name top_left color
          (List.nth cost col_i) g
  in
  List.fold_left
    (fun g col_i ->
      let top_left = plant_inv_point 4 origin row_i col_i in
      g
      |> draw_plant plant_layer_name plant top_left
      |> num_draw_f top_left col_i)
    gui
    (List.rev (List.init capacity Fun.id))

let draw_plant_inventory
    plant_layer_name
    cost_layer_name
    offset
    nums
    costs
    gui =
  let enumerate_stages =
    List.mapi (fun i s -> (i, s)) Plant.all_stages
  in
  List.fold_left
    (draw_plant_row plant_layer_name cost_layer_name gui.turn offset
       nums costs)
    gui
    (List.rev enumerate_stages)

let update_store num_bought gui =
  let capacities = List.map List.length gui.store_costs in
  gui
  |> draw_plant_inventory "store_plants" "store_costs"
       (get_offset "store" gui)
       capacities (Some gui.store_costs)

let update_available num_available gui =
  (* cost_layer_name is not used since costs is None, so set it to "" *)
  gui
  |> draw_plant_inventory "available" ""
       (get_offset "available" gui)
       num_available None

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

let init_gui cells player_params =
  let layer_names =
    [
      "background";
      "hexes";
      "cursor";
      "cells";
      "static text";
      "store_plants";
      "store_costs";
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
      store_costs =
        [ [ 1; 1; 2; 2 ]; [ 2; 2; 3; 3 ]; [ 3; 3; 4 ]; [ 4; 5 ] ];
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
