open HexMap
open HexUtil
open Board
open Gui

exception End

exception Invalid_Direction

type t = {
  current_position : HexUtil.coord;
  hexMap : HexMap.t;
  gui : Gui.t;
}

let init_state gui : t =
  {
    current_position = { col = 2; diag = 2 };
    hexMap = HexMap.init_map ();
    gui;
  }

let extract (c : HexUtil.coord option) : HexUtil.coord =
  match c with Some i -> i | None -> raise Invalid_Direction

let scroll s d =
  try
    let newcoord = HexMap.neighbor s.hexMap s.current_position d in
    let new_state =
      {
        current_position = extract newcoord;
        hexMap = s.hexMap;
        gui = let new_coord = HexMap.neighbor s.hexMap s.current_position d in  
        Gui.update_cursor ANSITerminal.Red new_coord s.gui;
      }
    in
    render new_state.gui; new_state
  with Invalid_Direction -> s

let handle_char s c =
  match c with
  | '&' -> raise End
  | 'w' -> scroll s 4
  | 'a' -> scroll s 2
  | 's' -> scroll s 1
  | 'd' -> scroll s 5
  | 'x' -> ()
  | _ -> failwith "Invalid Key Pressed"

let rec read_char (s : t) =
  Graphics.open_graph " ";
  try
    while true do
      try
        let a = Graphics.wait_next_event [ Graphics.Key_pressed ] in
        if a.Graphics.keypressed then
          let new_state = handle_char s a.Graphics.key in
          read_char new_state
        else read_char s
      with End -> raise End
    done
  with End -> failwith "Not Yet Implemented"
