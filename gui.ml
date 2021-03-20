type raster = {
  grid : char option list list;
  width : int;
  height : int;
}

type t = {
  width : int;
  height : int;
  layers : (string * raster) list;
  layer_order : string list;
  graphics : (string * raster) list;
}

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

let draw graphic x y layer =
  let row_draw grid_r graphic_r =
    map_offset
      (fun grid_char graphic_char -> graphic_char)
      grid_r graphic_r x
  in
  { graphic with grid = map_offset row_draw layer.grid graphic.grid y }

let update_layer layer_name f gui =
  let new_layer = f (List.assoc layer_name gui.layers) in
  {
    gui with
    layers =
      (layer_name, new_layer) :: List.remove_assoc layer_name gui.layers;
  }

let update_cells gui =
  gui |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 0 0)

(* |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 1 1) *)

(* |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 10 5)
   |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 98 28)
   |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 0 28)
   |> update_layer "hexes" (draw (List.assoc "hex" gui.graphics) 98 0) *)

let update_sun gui dir = gui

let fill_raster c w h =
  let rec fill_grid c w h =
    let rec fill_row c w =
      match w with 0 -> [] | n -> c :: fill_row c (w - 1)
    in
    match h with 0 -> [] | n -> fill_row c w :: fill_grid c w (h - 1)
  in
  { grid = fill_grid c w h; width = w; height = h }

(** [blank_raster w h] is a raster with a rectangular grid of [None]
    char options with width [w] and height [h]. *)
let blank_raster w h = fill_raster None w h

(** [null_raster] is a raster with an empty grid and 0 width and height. *)
let null_raster = blank_raster 0 0

(** [load_graphics none_c names] is an associative list mapping the
    [string] graphic names in [names] to the corresponding graphics,
    where each graphic is in the form of a [grid]. Each line
    ([char option list]) in the [grid] will contain elements up to, but
    not including the next ['\n']. The character [none_c] will be
    represented as [None], while all other characters [c] will be
    represented as [Some c]. It is possible that for the graphics to be
    represented by non-rectangular lists. *)
let load_graphics none_c names =
  let load_graphic name =
    let rec load_graphic_helper ic g_acc =
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
        with (* Close the *)
        | End_of_file -> (
          match l_acc with None -> None | Some a -> Some (List.rev a) )
      in
      match load_line ic None with
      | None -> List.rev g_acc
      | Some line -> load_graphic_helper ic (line :: g_acc)
    in
    let input_channel = open_in ("graphics/" ^ name ^ ".txt") in
    let result = load_graphic_helper input_channel [] in
    close_in input_channel;
    (name, { grid = result; width = 9; height = 5 })
  in
  List.map load_graphic names

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
      layers =
        [
          ("background", fill_raster (Some '#') w h);
          ("hexes", blank_raster w h);
        ];
      layer_order = [ "background"; "hexes" ];
      graphics =
        load_graphics ' ' [ "hex" ]
        (* [ ("hex", [ [ Some '/'; Some '\\' ]; [ Some '\\'; Some '/' ]
           ]); ]; *);
    }
  in
  update_cells gui

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
  { under with grid = List.map2 merge_two_rows under.grid over.grid }

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
  | [] -> null_raster
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
  let render_raster = merge_layers gui.layer_order gui.layers in
  print_grid render_raster.grid
