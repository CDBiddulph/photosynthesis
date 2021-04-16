open ANSITerminal
open Gui
open Ui

let basic_cell1 =
  Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Seed))

let basic_cell2 =
  Cell.init_cell 2 (Some (Plant.init_plant 2 Plant.Small))

let basic_cell3 =
  Cell.init_cell 3 (Some (Plant.init_plant 3 Plant.Medium))

let basic_cell4 =
  Cell.init_cell 4 (Some (Plant.init_plant 4 Plant.Large))

let player_params =
  [
    (1, ('s', ANSITerminal.Green));
    (2, ('c', ANSITerminal.Red));
    (3, ('x', ANSITerminal.Blue));
    (4, ('o', ANSITerminal.Yellow));
  ]

let main () =
  let hex_map = HexMap.init_map () in
  let gui = init_gui (HexMap.flatten hex_map) player_params in
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
  |> update_cursor ANSITerminal.Red (Some { diag = 0; col = 0 })
  |> update_cursor ANSITerminal.Red (Some { diag = 2; col = 2 })
  |> update_message "You shouldn't be able to see this" ANSITerminal.Red
  |> update_message "(P) Plant small tree" ANSITerminal.White
  |> render;
  let state = init_state gui in
  read_char state

let () = main ()
