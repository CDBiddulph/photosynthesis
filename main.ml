(* open ANSITerminal *)
open Gui

let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] "\n\nHello World"; *)
  let gui =
    init_gui [ { diag = 20; col = 10 }; { diag = 15; col = 15 } ]
    (* [ Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Seed)) ] *)
  in
  render gui

let () = main ()
