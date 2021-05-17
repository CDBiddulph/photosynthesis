open HexUtil
open Raster

type t = {
  size : point2d;
  layers : (string * (Raster.t * bool)) list;
  layer_order : string list;
  char_graphics : (string * char grid) list;
  color_graphics : (string * ANSITerminal.color grid) list;
}

exception Layer_not_found of string

exception Char_grid_not_found of string

exception Color_grid_not_found of string

let set_layer layer_name new_layer rend =
  let _, old_vis =
    try List.assoc layer_name rend.layers
    with Not_found -> raise (Layer_not_found layer_name)
  in
  {
    rend with
    layers =
      (layer_name, (new_layer, old_vis))
      :: List.remove_assoc layer_name rend.layers;
  }

let get_graphic_with_color_grid char_name color_name rend =
  let char_grid =
    try List.assoc char_name rend.char_graphics
    with Not_found -> raise (Char_grid_not_found char_name)
  in
  let color_grid =
    try List.assoc color_name rend.color_graphics
    with Not_found -> raise (Color_grid_not_found color_name)
  in
  { char_grid; color_grid }

let get_graphic_fill_color char_name color rend =
  let char_grid =
    try List.assoc char_name rend.char_graphics
    with Not_found -> raise (Char_grid_not_found char_name)
  in
  let color_grid = map_grid (fun _ -> Some color) char_grid in
  { char_grid; color_grid }

let get_layer name rend = List.assoc name rend.layers |> fst

let set_visible visible layer_name rend : t =
  {
    rend with
    layers =
      List.map
        (fun ((name, (l, _)) as layer) ->
          if name = layer_name then (name, (l, visible)) else layer)
        rend.layers;
  }

let init_rend layer_info size =
  (* layer_names must be in order from back to front, since it will be
     used to make layer_order *)
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
      "sun/0";
      "sun/1";
      "sun/2";
      "sun/3";
      "sun/4";
      "sun/5";
      "instructions";
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
  let layers =
    List.map
      (fun (name, vis) -> (name, (blank_raster size, vis)))
      layer_info
  in
  {
    size;
    layers;
    layer_order = List.map fst layer_info;
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
  let visible_layers =
    List.filter_map
      (fun (name, (layer, visible)) ->
        if visible then Some (name, layer) else None)
      rend.layers
  in
  let render_raster = merge_rasters rend.layer_order visible_layers in
  ignore (Sys.command "clear");
  print_raster render_raster

let blank_layer rend = blank_raster rend.size

let fill_layer rend = fill_raster rend.size
