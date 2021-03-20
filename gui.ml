type grid = char list list

type t = {
  width : int;
  height : int;
  layers : (string * grid) list;
}

let rec fill_layer c w h =
  let rec fill_row c w =
    match w with 0 -> [] | n -> c :: fill_row c (w - 1)
  in
  match h with 0 -> [] | n -> fill_row c w :: fill_layer c w (h - 1)

(** Creates a GUI with layers of grids. Postcondition: Each grid has the
    same dimensions. *)
let init_gui =
  let w = 100 in
  let h = 30 in
  {
    width = w;
    height = h;
    layers =
      [
        ("background", fill_layer 'X' w h);
        ("hexes", fill_layer '\x00' w h);
      ];
  }

let update_cells gui = gui

let update_sun gui dir = gui

let merge_two_layers under over =
  let merge_two_rows u_row o_row =
    List.map2 (fun u o -> if o = '\x00' then u else o) u_row o_row
  in
  List.map2 merge_two_rows under over

let merge_layers layers =
  let rec merge_layers_helper layers acc =
    match layers with [] -> acc | h :: t -> merge_two_layers acc h
  in
  match layers with
  | [] -> []
  | [ h ] -> h
  | h :: t -> merge_layers_helper t h

let render gui =
  let print_grid g =
    let print_row row = List.iter print_char row in
    List.iter
      (fun row ->
        print_row row;
        print_newline ())
      g
  in
  let render_grid =
    merge_layers (List.map (fun l -> snd l) gui.layers)
  in
  print_grid render_grid
