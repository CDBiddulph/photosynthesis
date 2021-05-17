open ANSITerminal
open Ui
open Gui

let player_params =
  [
    (1, ('s', ANSITerminal.Green));
    (2, ('c', ANSITerminal.Red));
    (3, ('x', ANSITerminal.Cyan));
    (4, ('o', ANSITerminal.Yellow));
  ]

let rec get_num_players () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Please enter the number of players\n> ";
  match read_line () with
  | exception End_of_file ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Invalid Input. Must be a number between 2 and 4 \n> ";
      get_num_players ()
  | str -> (
      if str = "" then 4
      else
        match int_of_string_opt str with
        | None ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Invalid Input. Must be a number between 2 and 4 \n> ";
            get_num_players ()
        | Some n ->
            if n = 2 || n = 3 || n = 4 then n
            else (
              ANSITerminal.print_string [ ANSITerminal.red ]
                "Invalid Input. Must be a number between 2 and 4 \n> ";
              get_num_players ()))

let rec get_ruleset () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Please enter the ruleset (Normal or Extended)\n> ";
  match read_line () with
  | exception End_of_file ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Invalid Input. Must be Normal or Extended \n> ";
      get_ruleset ()
  | str -> (
      match String.lowercase_ascii str with
      | "normal" | "n" | "" -> Game.Normal
      | "extended" | "e" -> Game.Extended
      | _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Invalid Input. Must be Normal or Extended \n> ";
          get_ruleset ())

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Photosynthesis\n";
  let num_players = get_num_players () in
  let ruleset = get_ruleset () in
  let game = Game.init_game num_players Board.Normal ruleset in
  let hex_map = HexMap.init_map () in
  let init_instr = false in
  let gui =
    init_gui
      [ [ 1; 1; 2; 2 ]; [ 2; 2; 3; 3 ]; [ 3; 3; 4 ]; [ 4; 5 ] ]
      [ 2; 4; 1; 0 ] [ 14; 17; 19; 22 ] (Some Ui.init_cursor) init_instr
      (Game.sun_dir game)
      (HexMap.flatten hex_map)
      [
        (1, ('o', Green));
        (2, ('s', Yellow));
        (3, ('c', Red));
        (4, ('x', Blue));
      ]
    |> Gui.update_message "Press (I) to open instructions"
         ANSITerminal.White
  in
  gui |> render;
  let state = Ui.init_state init_instr gui game in
  Ui.read_char state

let () = main ()
