open HexMap
open Graphics
open HexUtil
open Gui
open Board

exception End

type t = {
  current_position : HexUtil.coord;
  hexMap : HexMap.t;
}

let init_state () : t =
  { current_position = HexUtil.init_coord (); hexMap = HexMap.init_map }

let extract (c : HexUtil.coord option) : HexUtil.coord =
  match c with Some i -> i | None -> failwith "Invalid Direction"

let update_state s d =
  let new_st =
    {
      current_position =
        extract (HexMap.neighbor s.hexMap s.current_position d);
      hexMap = s.hexMap;
    }
  in
  new_st

(** let state = update_state s d in *)
let scroll s d = failwith "Not Implemented"

let handle_char s c =
  match c with
  | '&' -> raise End
  | 'w' -> scroll s 0
  | 'a' -> scroll s 1
  | 's' -> scroll s 2
  | 'd' -> scroll s 3
  | _ -> failwith "Invalid Key Pressed"

let read_char s f_end =
  try
    while true do
      try
        let s = Graphics.wait_next_event [ Graphics.Key_pressed ] in
        if s.Graphics.keypressed then handle_char s s.Graphics.key
      with End -> raise End
    done
  with End -> f_end ()
