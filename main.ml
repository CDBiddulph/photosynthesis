open ANSITerminal
open Gui

let basic_cell1 =
  Cell.init_cell 1 (Some (Plant.init_plant 1 'x' Cyan Plant.Small))

let basic_cell2 =
  Cell.init_cell 2 (Some (Plant.init_plant 2 'c' Red Plant.Medium))

let basic_cell3 =
  Cell.init_cell 3 (Some (Plant.init_plant 3 's' Green Plant.Large))

let basic_cell4 =
  Cell.init_cell 4 (Some (Plant.init_plant 4 'o' Yellow Plant.Small))

let main () =
  let gui =
    init_gui
      [
        basic_cell1 { diag = 0; col = 0 };
        basic_cell2 { diag = 1; col = 0 };
        basic_cell3 { diag = 0; col = 1 };
        basic_cell4 { diag = 1; col = 1 };
        basic_cell1 { diag = 0; col = 3 };
        basic_cell2 { diag = 3; col = 0 };
        basic_cell3 { diag = 3; col = 3 };
        basic_cell4 { diag = 6; col = 3 };
        basic_cell2 { diag = 6; col = 6 };
        basic_cell3 { diag = 3; col = 6 };
      ]
  in
  render gui

let () = main ()
