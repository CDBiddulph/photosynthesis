(* open ANSITerminal *)
open Gui
open Cell
open Plant

let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] "\n\nHello World" *)
  let gui = init_gui [] in
  print_endline "Hello World"

let () = main ()
