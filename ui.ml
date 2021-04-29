open Cell
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

let extract_plant (p : Plant.t option) = match p with Some i -> i |None -> failwith "Should Not Happen"

let num_remaining_available (p : Player.t) =
  Player.num_in_available Plant.Seed p :: Player.num_in_available Plant.Small p ::
  Player.num_in_available Plant.Medium p :: Player.num_in_available Plant.Large p :: []

let num_remaining_store (p : Player.t) =
  Player.num_in_store p Plant.Seed ::  Player.num_in_store p Plant.Small ::
  Player.num_in_store p Plant.Medium :: Player.num_in_store p Plant.Large :: []


let update_message (s : t) (p : HexUtil.coord) = 
  let pl = Game.player_of_turn s.game in 
  if Game.can_plant_seed p (Player.player_id pl) s.game then 
    "(P) Plant Seed"
  else if Game.can_plant_small p s.game then
    "(P) Plant Small Tree"
  else if Game.can_grow_plant p (Player.player_id pl) s.game then 
    let plnt_stg = Game.cell_at s.game p |> Cell.plant |> extract_plant |> Plant.plant_stage in 
    match plnt_stg with 
    | Seed -> "(P) Grow Small Tree"
    | Small -> "(P) Grow Medium Tree"
    | Medium -> "(P) Grow Tall Tree"
    | Large -> "(P) Collect Tall Tree"
  else "" 

let scroll s d =
    let map = (Game.board s.game) |> Board.map in
    let new_pos = HexMap.neighbor map s.current_position d in
    match new_pos with 
    | None ->  
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
    | Some pos -> 
      let new_gui =
        Gui.update_cursor new_pos s.gui
        |> Gui.update_message (update_message s pos) ANSITerminal.Green
      in
      render new_gui;
      let new_state =
        {
          current_position = pos;
          gui = new_gui;
          game = s.game
        }
      in
      new_state
  
let plant_helper s f p_id = 
  let pl_id = Game.player_of_turn s.game |> Player.player_id in 
  let new_game = f pl_id s.current_position s.game in 
  let new_gui = 
    let cells = Game.cells new_game in 
    Gui.update_cells cells s.gui |> Gui.update_message "" ANSITerminal.White in 
    render new_gui;
  let new_state = 
    {
      current_position = s.current_position;
      gui = new_gui;
      game = new_game;
    }
  in new_state 

let plant_helper_exn (s : t) f plnt_stg = 
  try
    let pl_id = Game.player_of_turn s.game |> Player.player_id in 
    let new_game = Game.buy_plant plnt_stg s.game |> f pl_id s.current_position in 
    let new_gui =
      let cells = Game.cells new_game in 
      Gui.update_cells cells s.gui |> Gui.update_message "" ANSITerminal.White in 
      render new_gui;
    let new_state = 
      {
        current_position = s.current_position; 
        gui = new_gui; 
        game = new_game;
      }
    in new_state
  with
  | Store.InsufficientLightPoints plnt_stg -> 
    let new_gui = Gui.update_message "Insufficient Light Points" ANSITerminal.Red s.gui in 
    render new_gui; 
    let new_state = 
      {
        current_position = s.current_position;
        gui = new_gui;
        game = s.game;
      } 
    in new_state
  | PlantInventory.OutOfPlant plnt_stg -> 
    let new_gui = Gui.update_message "Out of Plant" ANSITerminal.Red s.gui in 
    render new_gui;
    let new_state = 
      {
        current_position = s.current_position;
        gui = new_gui;
        game = s.game;
      }
    in new_state

let plant s = 
  try 
    let pl_id = Game.player_of_turn s.game |> Player.player_id in 
    if Game.can_plant_seed s.current_position pl_id s.game then
      plant_helper s Game.plant_seed pl_id
    else if Game.can_plant_small s.current_position s.game then 
      plant_helper s Game.plant_small pl_id
    else if Game.can_grow_plant s.current_position pl_id s.game then 
      let plnt_stg = Game.cell_at s.game s.current_position |> Cell.plant |> extract_plant |> Plant.plant_stage in 
      match plnt_stg with 
      | Seed -> plant_helper s Game.grow_plant pl_id
      | Small -> plant_helper s Game.grow_plant pl_id 
      | Medium -> plant_helper s Game.grow_plant pl_id
      | Large -> plant_helper s Game.harvest pl_id 
    else s  
  with 
  | Board.IllegalPlacePlant -> 
    let new_gui = 
      Gui.update_message "Illegal Placement of Plant" ANSITerminal.Red s.gui in 
    render new_gui;
    let new_state = 
      {
        current_position = s.current_position;
        gui = new_gui;
        game = s.game;
      }
    in new_state
  | PlantInventory.OutOfPlant Plant.Seed -> 
    plant_helper_exn s Game.plant_seed Plant.Seed 
  | PlantInventory.OutOfPlant Plant.Small ->
    plant_helper_exn s Game.plant_small Plant.Small
  | PlantInventory.OutOfPlant Plant.Medium ->
    plant_helper_exn s Game.grow_plant Plant.Medium
  | PlantInventory.OutOfPlant Plant.Large ->
    plant_helper_exn s Game.grow_plant Plant.Large

 let end_turn s = 
  let new_game = Game.end_turn s.game in
  let pl = Game.player_of_turn new_game in 
  let pl_id = Player.player_id pl in  
  let num_store_remaining = num_remaining_store pl in 
  let num_available = num_remaining_available pl in 
  let hlo = 
    let plnt_opt = s.current_position |> Game.cell_at s.game |> Cell.plant in 
    match plnt_opt with 
    | None -> None
    | Some plant -> 
      if Plant.player_id plant <> pl_id then None
      else 
        let plnt_stg= 
          plant |> Plant.plant_stage in
        if Player.is_in_available plnt_stg pl then Some (false, plnt_stg)
        else Some (true, plnt_stg) in
  let new_gui = Gui.update_turn pl_id (Player.light_points pl) (Player.score_points pl) num_store_remaining num_available hlo s.gui 
  |> Gui.update_sun (Game.sun_dir s.game) in 
  render new_gui;
  let new_state = 
    {
      current_position = s.current_position;
      game = new_game;
      gui = new_gui;
    }
  in new_state
  
let handle_char s c =
  match c with
  | 'q' -> scroll s 3
  | 'e' -> scroll s 5
  | 'w' -> scroll s 4
  | 'a' -> scroll s 2
  | 's' -> scroll s 1
  | 'd' -> scroll s 0
  | 'p' -> plant s
  | 'f' -> end_turn s
  | _ -> s

let rec read_char (s : t) =
  Graphics.open_graph " 100x100+900+0";
  while true do
    try
      let a = Graphics.wait_next_event [ Graphics.Key_pressed ] in
      if a.Graphics.keypressed then
        let new_state = handle_char s a.Graphics.key in
        read_char new_state
      else read_char s
    with End -> raise End
  done
