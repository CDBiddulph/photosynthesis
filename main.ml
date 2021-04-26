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

let main1 () =
  let gui =
    init_gui
      [
        basic_cell1 { diag = 0; col = 0 };
        basic_cell2 { diag = 1; col = 0 };
        basic_cell3 { diag = 0; col = 1 };
        basic_cell4 { diag = 1; col = 1 };
        basic_cell1 { diag = 0; col = 3 };
        soil_cell2 { diag = 3; col = 0 };
        soil_cell3 { diag = 3; col = 3 };
        soil_cell4 { diag = 6; col = 3 };
        basic_cell1 { diag = 6; col = 6 };
        basic_cell2 { diag = 3; col = 6 };
      ]
      [
        (1, ('o', Green));
        (2, ('s', Yellow));
        (3, ('c', Red));
        (4, ('x', Blue));
      ]
  in
  gui
  |> update_cells [ soil_cell1 { diag = 6; col = 6 } ]
  |> update_cursor ANSITerminal.Red (Some { diag = 4; col = 3 })
  |> update_cursor ANSITerminal.Red (Some { diag = 0; col = 0 })
  |> render

let main2 () =
  let hex_map = HexMap.init_map () in
  let gui =
    init_gui
      (HexMap.flatten hex_map)
      [
        (1, ('o', Green));
        (2, ('s', Yellow));
        (3, ('c', Red));
        (4, ('x', Blue));
      ]
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
  |> update_turn 4 [ 4; 2; 0; 1 ] [ 0; 3; 1; 1 ]
       (Some (true, Plant.Medium))
  |> update_turn 3 [ 2; 4; 1; 0 ] [ 2; 1; 0; 1 ]
       (Some (false, Plant.Small))
  |> render;
  let state = Ui.init_state gui in Ui.read_char state

let () = main2 ()
