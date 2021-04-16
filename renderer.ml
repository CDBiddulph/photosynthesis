open HexUtil
open Raster

type t = {
  size : point2d;
  layers : (string * Raster.t) list;
  layer_order : string list;
  char_graphics : (string * char grid) list;
  color_graphics : (string * ANSITerminal.color grid) list;
}

let apply_to_layer layer_name f rend =
  let new_layer = f (List.assoc layer_name rend.layers) in
  {
    rend with
    layers =
      (layer_name, new_layer)
      :: List.remove_assoc layer_name rend.layers;
  }

let get_graphic char_name color_name rend =
  {
    char_grid = List.assoc char_name rend.char_graphics;
    color_grid = List.assoc color_name rend.color_graphics;
  }

let init_rend size layer_names char_grid_names color_grid_names =
  (* layers must be in order from back to front, since it will be used
     to make layer_order *)
  let layers = List.map (fun n -> (n, blank_raster size)) layer_names in
  {
    size;
    layers;
    layer_order = layer_names;
    char_graphics = load_char_grids '`' char_grid_names;
    color_graphics = load_color_grids '`' color_grid_names;
  }

let render rend =
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
  let render_raster = merge_rasters rend.layer_order rend.layers in
  ignore (Sys.command "clear");
  print_raster render_raster

let blank_layer rend = blank_raster rend.size

let fill_layer rend = fill_raster rend.size
