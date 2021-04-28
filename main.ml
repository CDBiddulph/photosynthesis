open ANSITerminal
open Ui
open Gui

let basic_cell1 =
  Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Seed))

let basic_cell2 =
  Cell.init_cell 2 (Some (Plant.init_plant 2 Plant.Small))

let basic_cell3 =
  Cell.init_cell 3 (Some (Plant.init_plant 3 Plant.Medium))

let basic_cell4 =
  Cell.init_cell 4 (Some (Plant.init_plant 4 Plant.Small))

let player_params =
  [
    (1, ('s', ANSITerminal.Green));
    (2, ('c', ANSITerminal.Red));
    (3, ('x', ANSITerminal.Cyan));
    (4, ('o', ANSITerminal.Yellow));
  ]
let soil_cell1 = Cell.init_cell 1 None

let soil_cell2 = Cell.init_cell 2 None

let soil_cell3 = Cell.init_cell 3 None

let soil_cell4 = Cell.init_cell 4 None

let rec get_num_players () = 
  ANSITerminal.print_string [ ANSITerminal.red ]
  "Please enter the number of players\n> ";
  match read_line () with 
  | exception End_of_file -> ANSITerminal.print_string [ ANSITerminal.red ]
  "Invalid Input. Must be a number between 2 and 4 \n> "; get_num_players ()
  | str ->
    if str = "" then 4 else
    match int_of_string_opt str with 
    | None -> ANSITerminal.print_string [ ANSITerminal.red ]
    "Invalid Input. Must be a number between 2 and 4 \n> "; get_num_players ()
    | Some n -> 
      if n = 2 || n = 3 || n = 4 then n
      else (ANSITerminal.print_string [ ANSITerminal.red ]
      "Invalid Input. Must be a number between 2 and 4 \n> "; get_num_players ())

let rec get_ruleset () = 
  ANSITerminal.print_string [ ANSITerminal.red ]
  "Please enter the ruleset (Normal or Extended)\n> ";
  match read_line () with 
  | exception End_of_file -> ANSITerminal.print_string [ ANSITerminal.red ]
  "Invalid Input. Must be Normal or Extended \n> "; get_ruleset ()
  | str ->
    match String.lowercase_ascii str with 
    | "normal" | "n" | "" -> Board.Normal
    | "extended" | "e" -> Board.Extended
    | _ -> 
      ANSITerminal.print_string [ ANSITerminal.red ]
  "Invalid Input. Must be Normal or Extended \n> "; get_ruleset ()

let main1 () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Photosynthesis\n";
  let num_players = get_num_players () in 
  let ruleset = get_ruleset () in 
  let game = Game.init_game num_players ruleset in 
  let hex_map = HexMap.init_map () in
  let gui =
    init_gui
      [ [ 1; 1; 2; 2 ]; [ 2; 2; 3; 3 ]; [ 3; 3; 4 ]; [ 4; 5 ] ]
      [ 2; 4; 1; 0 ] [ 14; 17; 19; 22 ]
      (HexMap.flatten hex_map)
      [
        (1, ('o', Green));
        (2, ('s', Yellow));
        (3, ('c', Red));
        (4, ('x', Blue));
      ]
  in
  gui |> update_cursor (Some { diag = 2; col = 2 }) |> render;
  let state = Ui.init_state gui game in
  Ui.read_char state
let main () =
  let hex_map = HexMap.init_map () in
  let gui =
    init_gui
      [ [ 1; 1; 2; 2 ]; [ 2; 2; 3; 3 ]; [ 3; 3; 4 ]; [ 4; 5 ] ]
      [ 2; 4; 1; 0 ] [ 14; 17; 19; 22 ]
      (HexMap.flatten hex_map)
      player_params
  in
  gui
  |> update_cells
       [
         basic_cell1 { diag = 6; col = 6 };
         basic_cell2 { diag = 4; col = 2 };
         basic_cell3 { diag = 0; col = 3 };
         basic_cell4 { diag = 2; col = 4 };
         basic_cell1 { diag = 5; col = 3 };
         basic_cell2 { diag = 1; col = 3 };
         basic_cell3 { diag = 3; col = 3 };
         basic_cell4 { diag = 0; col = 0 };
       ]
  |> update_sun 5 |> update_sun 0
  |> update_cursor (Some { diag = 0; col = 0 })
  |> update_cursor (Some { diag = 2; col = 2 })
  |> update_message "You shouldn't be able to see this" ANSITerminal.Red
  |> update_message "(P) Plant small tree" ANSITerminal.White
  |> update_turn 4 20 7 [ 4; 2; 0; 1 ] [ 0; 3; 1; 1 ]
       (Some (false, Plant.Small))
  |> update_turn 3 5 11 [ 2; 4; 1; 0 ] [ 2; 1; 0; 1 ]
       (Some (true, Plant.Seed))
  |> update_cell_highlight [ { diag = 2; col = 4 } ]
  |> update_cell_highlight
       [
         { diag = 6; col = 6 };
         { diag = 4; col = 2 };
         { diag = 0; col = 3 };
       ]
  |> update_next_sp 1 1 |> update_next_sp 2 9 |> update_next_sp 3 10
  |> update_player_lp 20 |> update_player_sp 100 |> update_player_sp 19
  |> photosynthesis
       [
         (1, [ ({ diag = 7; col = 6 }, 3) ]);
         (2, [ ({ diag = 5; col = 2 }, 2); ({ diag = 2; col = 3 }, 1) ]);
       ]
  |> clear_photosynthesis
  |> photosynthesis
       [
         (1, [ ({ diag = 6; col = 6 }, 3) ]);
         (2, [ ({ diag = 4; col = 2 }, 2); ({ diag = 1; col = 3 }, 1) ]);
       ]
  |> render
 

let () = main1 ()
