open Plant
open HexMap
open HexUtil
open Board
open Gui
open Graphics
open Game
open Player

exception End

exception Invalid_Direction
exception Invalid_Cell

type t = {
  current_position : HexUtil.coord;
  game : Game.t;
  gui : Gui.t;
}

let init_state (gui : Gui.t) (game : Game.t): t =
  {
    current_position = { col = 2; diag = 2 };
    game = game;
    gui = gui;
  }

let extract (c : HexUtil.coord option) : HexUtil.coord =
  match c with Some i -> i | None -> raise Invalid_Direction

let extract_cell (c : Cell.t option) : Cell.t = 
  match c with Some i -> i | None -> raise Invalid_Cell

let extract_plant p = match p with Some i -> i |None -> failwith "Should Not Happen"

let update_message (s : t) = 
  let pl = Game.player_of_turn s.game in 
  if Game.can_plant_seed s.current_position (Player.player_id pl) s.game then 
    "(P) Plant Seed"
  else if Game.can_plant_small s.current_position (Player.player_id pl) s.game then
    "(P) Plant Small Tree"
  else if Game.can_grow_plant s.current_position (Player.player_id pl) s.game then 
    let plnt_stg = Game.cell_at s.game s.current_position |> Cell.plant |> extract_plant |> Plant.plant_stage in 
    match plnt_stg with 
    | Seed -> failwith "Should Not Happen"
    | Small -> "(P) Grow Medium Tree"
    | Medium -> "(P) Grow Tall Tree"
    | Large -> "(P) Collect Tall Tree"
  else "" 

let scroll s d =
  try
    let new_map = (Game.board s.game) |> Board.map in
    let new_gui =
      Gui.update_cursor
        (HexMap.neighbor new_map s.current_position d)
        s.gui |> Gui.update_message (update_message s) ANSITerminal.White 
    in
    render new_gui;
    let new_state =
      {
        current_position =
          extract (HexMap.neighbor new_map s.current_position d);
        gui = new_gui;
        game = s.game
      }
    in
    new_state
  with Invalid_Direction -> 
    let new_gui = Gui.update_message "Invalid Direction" ANSITerminal.Red s.gui in
    render new_gui; 
    let new_state =
      {
        current_position = s.current_position;
        gui = new_gui;
        game = s.game;
      }
    in 
    new_state

let remove s = failwith "Not Implemented"

let handle_char s c =
  match c with
  | 'q' -> scroll s 3
  | 'e' -> scroll s 5
  | 'w' -> scroll s 4
  | 'a' -> scroll s 2
  | 's' -> scroll s 1
  | 'd' -> scroll s 0
  | _ -> s

let rec read_char (s : t) =
  Graphics.open_graph " ";
  while true do
    try
      let a = Graphics.wait_next_event [ Graphics.Key_pressed ] in
      if a.Graphics.keypressed then
        let new_state = handle_char s a.Graphics.key in
        read_char new_state
      else read_char s
    with End -> raise End
  done
