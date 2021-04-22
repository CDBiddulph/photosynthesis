open Renderer
open Raster
open HexUtil

type t = {
  rend : Renderer.t;
  offsets : (string * point2d) list;
  player_params : (PlayerId.t * (char * ANSITerminal.color)) list;
  turn : PlayerId.t;
  store_costs : int list list;
  num_store_remaining : int list;
  num_available : int list;
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
let draw_hexes layer_name color points gui =
  let hex_graphic = gui.rend |> get_graphic_fill_color "hex" color in
  List.fold_left (draw_at_point layer_name hex_graphic) gui points

(** [draw_soil gui point soil layer] returns [layer] with a soil marker
    for [soil] drawn on it in the position corresponding to [point] with
    an offset of [gui.hex_offset] + [gui.board_offset]. *)
let draw_soil layer_name soil point gui =
  let char_grid_name = "soil/" ^ string_of_int soil in
  let soil_graphic =
    get_graphic_fill_color char_grid_name ANSITerminal.Green gui.rend
  in
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
  |> get_graphic_with_color_grid char_name color_name
  |> replace_char 'x' (render_char player_id gui)
  |> replace_color ANSITerminal.Default (render_color player_id gui)

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
      let graphic = gui.rend |> get_graphic_fill_color "hex" color in
      draw_at_point layer_name graphic gui
        (point2d_of_hex_coord gui point)

let update_cursor coord_opt gui =
  let layer_name = "cursor" in
  gui |> set_blank layer_name
  |> draw_cursor layer_name ANSITerminal.Red coord_opt

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

let update_sun dir gui =
  let gui' = set_blank "sun" gui in
  draw_at_point "sun"
    (get_graphic_fill_color
       ("sun/" ^ string_of_int dir)
       ANSITerminal.Yellow gui'.rend)
    gui' (get_offset "sun" gui')

let draw_plant_num layer_name point color num gui =
  gui
  |> draw_text layer_name
       (point +: get_offset "plant_num" gui)
       (string_of_int num) color

let plant_inv_point capacity origin row_i col_i =
  if col_i >= capacity || col_i < 0 then None
  else
    let max_capacity = 4 in
    let x = 8 * (max_capacity - capacity + col_i) in
    (* Equivalent to taking the sum from 4 to row_i + 4 *)
    let y = ((row_i * row_i) + (7 * row_i)) / 2 in
    Some (origin +: { x; y })

let draw_row layer_name origin nums capacities gui (row_i, graphic) =
  let capacity = List.nth capacities row_i in
  let num = List.nth nums row_i in
  List.fold_left
    (fun g col_i ->
      match plant_inv_point capacity origin row_i col_i with
      | None -> failwith "Invalid args to plant_inv_point"
      | Some top_left -> draw_at_point layer_name graphic g top_left)
    gui
    (List.rev (List.init num Fun.id))

let get_capacities gui = List.map List.length gui.store_costs

let get_diffs_opt gone_opt capacities =
  match gone_opt with
  | None -> capacities
  | Some gone -> List.map2 ( - ) capacities gone

let draw_plant_inventory
    layer_name
    offset
    capacities
    gone_opt
    graphic_f
    gui =
  let nums = get_diffs_opt gone_opt capacities in
  let enumerate_graphics =
    List.mapi
      (fun i s ->
        (i, plant_graphic (Plant.init_plant gui.turn s) gui |> graphic_f))
      Plant.all_stages
  in
  List.fold_left
    (draw_row layer_name offset nums capacities)
    gui
    (List.rev enumerate_graphics)

let draw_costs layer_name offset color gone_opt gui =
  let capacities = get_capacities gui in
  let nums = get_diffs_opt gone_opt capacities in
  let indexed_costs =
    List.mapi
      (fun row_i cost_row ->
        List.mapi (fun col_i cost -> (row_i, col_i, cost)) cost_row)
      gui.store_costs
    |> List.flatten
    |> List.filter (fun (row_i, col_i, _) ->
           List.nth nums row_i > col_i)
  in
  List.fold_left
    (fun g (row_i, col_i, cost) ->
      let point =
        match
          plant_inv_point
            (List.nth gui.store_costs row_i |> List.length)
            offset row_i col_i
        with
        | None -> failwith "Invalid args to plant_inv_point"
        | Some p -> p
      in
      draw_plant_num layer_name point color cost g)
    gui indexed_costs

let draw_bought layer_name offset color num_remaining gui =
  gui |> set_blank layer_name
  |> draw_plant_inventory layer_name offset (get_capacities gui)
       (Some num_remaining) (fun g ->
         g |> replace_all_color color |> replace_char_with_none ' ')
  |> draw_costs layer_name offset color (Some num_remaining)

let update_store_remaining num_remaining gui =
  let new_gui = { gui with num_store_remaining = num_remaining } in
  new_gui
  |> draw_bought "store_bought"
       (get_offset "store" new_gui)
       ANSITerminal.Magenta num_remaining

let update_available num_available gui =
  let new_gui = { gui with num_available } in
  new_gui |> set_blank "available"
  |> draw_plant_inventory "available"
       (get_offset "available" new_gui)
       num_available None
       (replace_char_with_none ' ')

let draw_static_text layer_name gui =
  let draw_plant_inventory_static_text offset_name title =
    draw_text_lines layer_name
      (get_offset offset_name gui +: { x = 2; y = 1 })
      [ title; "------------------------------" ]
      ANSITerminal.White
  in
  gui
  |> draw_plant_inventory_static_text "store" "Store"
  |> draw_plant_inventory_static_text "available" "Available"

let draw_plant_highlight layer_name color loc_opt gui =
  match loc_opt with
  | None -> gui
  | Some (is_store, stage) -> (
      let top_left =
        get_offset (if is_store then "store" else "available") gui
      in
      let row_i = Plant.int_of_plant_stage stage in
      let capacity =
        List.nth
          (if is_store then get_capacities gui else gui.num_available)
          row_i
      in
      let col_i =
        if is_store then
          capacity - List.nth gui.num_store_remaining row_i
        else 0
      in
      match plant_inv_point capacity top_left row_i col_i with
      | None -> gui (* Don't update gui if this row has no plants *)
      | Some point ->
          let graphic =
            plant_graphic (Plant.init_plant gui.turn stage) gui
            |> replace_all_color color
            |> replace_char_with_none ' '
          in
          let gui' = draw_at_point layer_name graphic gui point in
          if is_store then
            let cost =
              List.nth (List.nth gui.store_costs row_i) col_i
            in
            draw_plant_num layer_name point color cost gui'
          else gui')

let update_plant_highlight loc_opt gui =
  let layer_name = "plant_highlight" in
  gui |> set_blank layer_name
  |> draw_plant_highlight layer_name ANSITerminal.White loc_opt

let update_cell_highlight coords gui =
  let layer_name = "cell_highlight" in
  gui |> set_blank layer_name
  |> draw_hexes layer_name ANSITerminal.Green
       (List.map (point2d_of_hex_coord gui) coords)

let draw_player_sign layer_name player_id gui =
  draw_text layer_name
    (get_offset "player_sign" gui)
    ("Player " ^ string_of_int player_id)
    (render_color player_id gui)
    gui

let update_turn
    player_id
    num_store_remaining
    num_available
    highlight_loc_opt
    gui =
  let store_offset = get_offset "store" gui in
  let new_turn_gui = { gui with turn = player_id } in
  new_turn_gui
  |> draw_plant_inventory "store_plants" store_offset
       (get_capacities gui) None
       (replace_char_with_none ' ')
  |> draw_costs "store_plants" store_offset
       (render_color new_turn_gui.turn new_turn_gui)
       None
  |> draw_player_sign "player_sign" player_id
  |> update_store_remaining num_store_remaining
  |> update_available num_available
  |> update_plant_highlight highlight_loc_opt

let init_gui store_costs init_available cells player_params =
  let layer_names =
    [
      "background";
      "sun";
      "hexes";
      "cell_highlight";
      "cursor";
      "cells";
      "store_plants";
      "store_bought";
      "available";
      "plant_highlight";
      "static text";
      "player_sign";
      "message";
    ]
  in
  let gui =
    {
      rend = init_rend layer_names { x = 119; y = 44 };
      offsets =
        [
          ("board", { x = 5; y = 2 });
          ("store", { x = 85; y = 3 });
          ("available", { x = 85; y = 24 });
          ("plant_num", { x = 5; y = 5 });
          ("player_sign", { x = 109; y = 2 });
          ("sun", { x = 3; y = 1 });
        ];
      player_params;
      turn = PlayerId.first;
      store_costs;
      num_store_remaining = List.map List.length store_costs;
      num_available = init_available;
    }
  in
  gui
  (* |> set_layer "background" (fill_layer gui.rend (Some '.') (Some
     ANSITerminal.Magenta)) *)
  |> draw_hexes "hexes" ANSITerminal.White
       (List.map
          (fun c -> c |> Cell.coord |> point2d_of_hex_coord gui)
          cells)
  |> draw_cells "cells" cells
  |> draw_static_text "static text"
  |> update_turn gui.turn gui.num_store_remaining gui.num_available None

let render gui = render gui.rend
