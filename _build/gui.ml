type grid = char option list list

type t = {
  width : int;
  height : int;
  layers : (string * grid) list;
  layer_order : string list;
  graphics : (string * grid) list;
}

let map_offset f big_lst small_lst offset =
  assert (offset >= 0);
  let rec map_offset_helper f big_lst small_lst offset acc =
    match small_lst with
    | [] -> acc @ big_lst
    | sl_h :: sl_t -> (
        match offset with
        | 0 -> (
            match big_lst with
            | [] ->
                failwith
                  "offset + length small_lst exceeds length big_lst"
            | bl_h :: bl_t ->
                map_offset_helper f bl_t sl_t 0 (acc @ [ f bl_h sl_h ])
            )
        | o -> (
            (* print_int o; print_newline (); *)
            match big_lst with
            | [] -> failwith "offset exceeds length big_lst"
            | bl_h :: bl_t ->
                map_offset_helper f bl_t small_lst (o - 1)
                  (acc @ [ bl_h ]) ) )
  in
  map_offset_helper f big_lst small_lst offset []

let draw graphic x y grid =
  let row_draw grid_r graphic_r =
    map_offset
      (fun grid_char graphic_char -> graphic_char)
      grid_r graphic_r x
  in
  map_offset row_draw grid graphic y

let update_layer layer_name f gui =
  let new_layer = f (List.assoc layer_name gui.layers) in
  {
    gui with
    layers =
      (layer_name, new_layer) :: List.remove_assoc layer_name gui.layers;
  }

let update_cells gui =
  gui
  |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 0 0)
  |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 1 1)
  |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 10 10)
  |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 98 28)
  |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 0 28)
  |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 98 0)

let update_sun gui dir = gui

let rec fill_layer c w h =
  let rec fill_row c w =
    match w with 0 -> [] | n -> c :: fill_row c (w - 1)
  in
  match h with 0 -> [] | n -> fill_row c w :: fill_layer c w (h - 1)

let empty_layer gui = fill_layer None gui.width gui.height

(** [init_gui ()] is a GUI with the layers of grids necessary to run the
    game. Postcondition: Each grid in [(init_gui ()).layers] has the
    same dimensions. *)
let init_gui =
  let w = 100 in
  let h = 30 in
  let gui =
    {
      width = w;
      height = h;
      layers = [];
      layer_order = [ "background"; "hexes" ];
      graphics =
        [
          ("hex", [ [ Some '/'; Some '\\' ]; [ Some '\\'; Some '/' ] ]);
        ];
    }
  in
  let gui' =
    {
      gui with
      layers =
        [
          ("background", fill_layer (Some '#') w h);
          ("hexes", empty_layer gui);
        ];
    }
  in
  update_cells gui'

(** (Deprecated) [past_n lst n] is the list containing the contents of
    [lst] including and after index [n]. Returns [\[\]] if
    [n = length lst]. Requires [0 <= n <= length lst]. *)
let past_n n lst =
  assert (n >= 0);
  match n with
  | 0 -> lst
  | n -> (
      match lst with
      | [] -> failwith "n exceeds length of lst"
      | h :: t -> t )

let merge_two_layers under over =
  let merge_two_rows u_row o_row =
    List.map2 (fun u o -> match o with None -> u | o -> o) u_row o_row
  in
  List.map2 merge_two_rows under over

(* let merge_layers layers = let rec merge_layers_helper layers acc =
   match layers with [] -> acc | h :: t -> merge_two_layers acc h in
   match layers with | [] -> [] | [ h ] -> h | h :: t ->
   merge_layers_helper t h *)
let merge_layers layer_order layers =
  let rec merge_layers_helper layer_order layers acc =
    match layer_order with
    | [] -> acc
    | h :: t -> merge_two_layers acc (List.assoc h layers)
  in
  match layer_order with
  | [] -> []
  | [ h ] -> List.assoc h layers
  | h :: t -> merge_layers_helper t layers (List.assoc h layers)

let render gui =
  let print_grid g =
    let print_row row =
      List.iter
        (fun co ->
          print_char (match co with None -> ' ' | Some c -> c))
        row
    in
    List.iter
      (fun row ->
        print_row row;
        print_newline ())
      g
  in
  let render_grid = merge_layers gui.layer_order gui.layers in
  print_grid render_grid
