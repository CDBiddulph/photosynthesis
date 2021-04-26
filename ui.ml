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

let update_message_helper (s : t) (stage : Plant.plant_stage) (pl : Player.t) : string = 
  let buy = 
    match stage with 
    | Plant.Seed -> 1
    | Plant.Small -> 1 
    | Plant.Medium -> 2
    | Plant.Large -> 3 in
  let cost = Store.cost (Player.store pl) stage in 
  if (Player.is_in_available stage pl) then 
    "Plant " ^ (Plant.to_string stage) ^ " for " ^ string_of_int buy ^ " lp"
  else "Buy and Plant " ^ (Plant.to_string stage) ^ " for " string_of_int (cost + buy) 

let update_message (s : t) : string = 
  let pl = Game.player_of_turn s.game in 
  if Game.can_plant_seed s.current_position (Player.player_id pl) s.game then
    update_message_helper s Plant.Seed
  else if Game.can_plant_small s.current_position (Player.player_id pl) s.game then
    update_message_helper s Plant.Small
  else if Game.can_grow_plant s.current_position (Player.player_id pl) s.game then
    let plnt_stg = Game.cell_at s.game |> Cell.plant_stage |> extract_plant in 
    match plnt_stg with 
    | Plant.Seed -> failwith "Should Not Happen"
    | Plant.Small -> 
      update_message_helper s Plant.Medium pl
    | Plant.Medium -> 
      update_message_helper s Plant.Large pl
    | Plant.Large -> 
      "Collect Tree for 4 lp"
  else ""

let scroll s d =
  try
    let new_map = Board.map (Game.board s.game) in
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

let plant_helper (s : t) (f : PlayerId.t -> HexUtil.coord -> Game.t -> Game.t) : t =
  let pl_id = Player.player_id (Game.player_of_turn s.game) in
  let new_game = f pl_id s.current_position s.game in 
  let new_gui = Gui.update_cells [Game.cell_at s.game s.current_position] in 
  render new_gui; 
  let new_state = 
    {
      current_position = s.current_position;
      game = new_game;
      gui = new_gui;
    }
  in new_state

let plant s = 
  let pl_id = Player.player_id (Game.player_of_turn s.game) in
  try
    if Game.can_plant_seed s.current_position pl_id s.game
      then
        plant_helper s (Game.plant_seed) 
    else if Game.can_plant_small s.current_position pl_id s.game
      then plant_helper s (Game.plant_small)
    else if Game.can_grow_plant s.current_position pl_id s.game
      then plant_helper s (Game.grow_plant)
    else s
  with
  | PlantInventory.OutOfPlant Plant.Seed -> 
    let new_gui = Gui.update_message "Out of Seeds" ANSITerminal.Red s.gui in
    render new_gui; 
    let new_state =
      {
        current_position = s.current_position;
        gui = new_gui;
        game = s.game;
      }
    in 
    new_state
  | PlantInventory.OutOfPlant Plant.Small ->
    let new_gui = Gui.update_message "Out of Small Trees" ANSITerminal.Red s.gui in
    render new_gui; 
    let new_state =
      {
        current_position = s.current_position;
        gui = new_gui;
        game = s.game;
      }
    in 
    new_state
  | PlantInventory.OutOfPlant Plant.Medium -> 
    let new_gui = Gui.update_message "Out of Medium Trees" ANSITerminal.Red s.gui in
    render new_gui; 
    let new_state =
      {
        current_position = s.current_position;
        gui = new_gui;
        game = s.game;
      }
    in 
    new_state
  | PlantInventory.OutOfPlant Plant.Large ->
    let new_gui = Gui.update_message "Out of Large Trees" ANSITerminal.Red s.gui in
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
  | 'p' -> plant s
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
