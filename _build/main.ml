(* open ANSITerminal *)
open Gui

let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] "\n\nHello World"; *)
  let gui =
    init_gui
    (* [ init_cell 1 (Some (init_plant 1 Plant.Seed)) ] *)
  in
  render gui

let () = main ()
