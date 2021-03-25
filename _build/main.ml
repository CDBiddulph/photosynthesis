open ANSITerminal
open Gui

let basic_hex =
  Cell.init_cell 1 (Some (Plant.init_plant 1 'x' Red Plant.Small))

let main () =
  let gui =
    init_gui
      [
        basic_hex { diag = 0; col = 0 };
        basic_hex { diag = 1; col = 0 };
        basic_hex { diag = 0; col = 1 };
        basic_hex { diag = 1; col = 1 };
        basic_hex { diag = 0; col = 3 };
        basic_hex { diag = 3; col = 0 };
        basic_hex { diag = 3; col = 3 };
        basic_hex { diag = 6; col = 3 };
        basic_hex { diag = 6; col = 6 };
        basic_hex { diag = 3; col = 6 };
      ]
  in
  render gui

let () = main ()
