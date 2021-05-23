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

let rec num_players_of_string str =
  if str = "" then Some 4
  else
    match int_of_string_opt str with
    | None -> None
    | Some n -> if n = 2 || n = 3 || n = 4 then Some n else None

and get_num_players () =
  let inv_message =
    "Invalid Input. Must be a number between 2 and 4\n>"
  in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Please enter the number of players\n> ";
  match read_line () with
  | exception End_of_file ->
      ANSITerminal.print_string [ ANSITerminal.red ] inv_message;
      get_num_players ()
  | str -> (
      match num_players_of_string str with
      | None ->
          ANSITerminal.print_string [ ANSITerminal.red ] inv_message;
          get_num_players ()
      | Some n -> n)

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

let main_init_gui init_instr game sun_revs hex_map =
  init_gui
    [ [ 1; 1; 2; 2 ]; [ 2; 2; 3; 3 ]; [ 3; 3; 4 ]; [ 4; 5 ] ]
    [ 2; 4; 1; 0 ] [ 14; 17; 19; 22 ] (Some Ui.init_cursor) init_instr
    (Game.sun_dir game) sun_revs
    (HexMap.flatten hex_map)
    [
      (1, ('o', Green));
      (2, ('s', Yellow));
      (3, ('c', Red));
      (4, ('x', Blue));
    ]
  |> Gui.update_message "Press (I) to open instructions"
       ANSITerminal.White

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Photosynthesis\n";
  let num_players = get_num_players () in
  let ruleset = get_ruleset () in
  let game = Game.init_game num_players Board.Normal ruleset in
  let sun_revs = match ruleset with Normal -> 3 | Extended -> 4 in
  let hex_map = HexMap.init_map () in
  let init_instr = false in
  let gui = main_init_gui init_instr game sun_revs hex_map in
  gui |> render;
  let state = Ui.init_state init_instr gui game in
  Ui.read_char state

let () = main ()
