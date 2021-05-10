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

let init_state (gui : Gui.t) (game : Game.t) : t =
  { current_position = { col = 2; diag = 2 }; game; gui }

let extract (c : HexUtil.coord option) : HexUtil.coord =
  match c with Some i -> i | None -> raise Invalid_Direction

let extract_cell (c : Cell.t option) : Cell.t =
  match c with Some i -> i | None -> raise Invalid_Cell

let extract_plant (p : Plant.t option) =
  match p with Some i -> i | None -> failwith "Should Not Happen"

let num_remaining_available (p : Player.t) =
  List.map
    (fun stage -> Player.num_in_available stage p)
    Plant.all_stages

let num_remaining_store (p : Player.t) =
  List.map (fun stage -> Player.num_in_store stage p) Plant.all_stages

let update_message (s : t) (coord : HexUtil.coord) =
  if
    Game.can_plant_seed coord
      (Game.player_of_turn s.game |> Player.player_id)
      s.game
  then "(P) Plant Seed"
  else if Game.can_plant_small coord s.game then "(P) Plant Small Tree"
  else if Game.can_grow_plant coord s.game then
    let plnt_stg =
      Game.cell_at s.game coord
      |> Cell.plant |> extract_plant |> Plant.plant_stage
    in
    match plnt_stg with
    | Seed -> "(P) Grow Small Tree"
    | Small -> "(P) Grow Medium Tree"
    | Medium -> "(P) Grow Tall Tree"
    | Large -> failwith "Should Not Happen"
  else if Game.can_harvest coord s.game then "(P) Harvest Tall Tree"
  else ""

let scroll s d =
  let map = Game.board s.game |> Board.map in
  let new_pos = HexMap.neighbor map s.current_position d in
  let pl = Game.player_of_turn s.game in
  let pl_id = Player.player_id pl in
  match new_pos with
  | None ->
      let new_gui =
        Gui.update_message "Invalid Direction" ANSITerminal.Red s.gui
      in
      let new_state = { s with gui = new_gui } in
      new_state
  | Some pos ->
      let hlo =
        let plnt_opt = pos |> Game.cell_at s.game |> Cell.plant in
        match plnt_opt with
        | None -> None
        | Some plant ->
            if Plant.player_id plant <> pl_id then None
            else
              let plnt_stg = plant |> Plant.plant_stage in
              if Player.is_in_available plnt_stg pl then
                Some (false, plnt_stg)
              else Some (true, plnt_stg)
      in
      let new_gui =
        Gui.update_cursor new_pos s.gui
        |> Gui.update_message (update_message s pos) ANSITerminal.White
        |> Gui.update_plant_highlight hlo
      in
      let new_state =
        { s with current_position = pos; gui = new_gui }
      in
      new_state

let plant_helper s f =
  let new_game = f s.current_position s.game in
  let pl = Game.player_of_turn new_game in
  let pl_id = Player.player_id pl in
  let num_store_remaining = num_remaining_store pl in
  let num_available = num_remaining_available pl in
  let hlo =
    let plnt_opt =
      s.current_position |> Game.cell_at new_game |> Cell.plant
    in
    match plnt_opt with
    | None -> None
    | Some plant ->
        if Plant.player_id plant <> pl_id then None
        else
          let plnt_stg = plant |> Plant.plant_stage in
          if Player.is_in_available plnt_stg pl then
            Some (false, plnt_stg)
          else Some (true, plnt_stg)
  in
  let new_gui =
    let cells = Game.cells new_game in
    Gui.update_cells cells s.gui
    |> Gui.update_message
         (update_message s s.current_position)
         ANSITerminal.White
    |> Gui.update_available num_available
    |> Gui.update_store_remaining num_store_remaining
    |> Gui.update_player_lp (Player.light_points pl)
    |> Gui.update_player_sp (Player.score_points pl)
    |> Gui.update_plant_highlight hlo
  in
  let new_state = { s with gui = new_gui; game = new_game } in
  new_state

let plant_helper_exn (s : t) f plnt_stg =
  try
    let new_game =
      Game.buy_plant plnt_stg s.game |> f s.current_position
    in
    let pl = Game.player_of_turn new_game in
    let pl_id = Player.player_id pl in
    let num_store_remaining = num_remaining_store pl in
    let num_available = num_remaining_available pl in
    let hlo =
      let plnt_opt =
        s.current_position |> Game.cell_at new_game |> Cell.plant
      in
      match plnt_opt with
      | None -> None
      | Some plant ->
          if Plant.player_id plant <> pl_id then None
          else
            let plnt_stg = plant |> Plant.plant_stage in
            if Player.is_in_available plnt_stg pl then
              Some (false, plnt_stg)
            else Some (true, plnt_stg)
    in
    let new_gui =
      let cells = Game.cells new_game in
      Gui.update_cells cells s.gui
      |> Gui.update_message "" ANSITerminal.White
      |> Gui.update_available num_available
      |> Gui.update_store_remaining num_store_remaining
      |> Gui.update_player_lp (Player.light_points pl)
      |> Gui.update_player_sp (Player.score_points pl)
      |> Gui.update_plant_highlight hlo
    in
    let new_state =
      {
        current_position = s.current_position;
        gui = new_gui;
        game = new_game;
      }
    in
    new_state
  with
  | Store.InsufficientLightPoints cost ->
      let new_gui =
        Gui.update_message
          ("Action requires " ^ string_of_int cost ^ " light points")
          ANSITerminal.Red s.gui
      in
      { s with gui = new_gui }
  | PlantInventory.OutOfPlant plnt_stg ->
      let new_gui =
        Gui.update_message
          ("Out of Plant "
          ^ (plnt_stg |> Plant.string_of_plant_stage
           |> String.capitalize_ascii))
          ANSITerminal.Red s.gui
      in
      { s with gui = new_gui }

let end_turn s =
  let new_game = Game.end_turn s.game in
  let pl = Game.player_of_turn new_game in
  let pl_id = Player.player_id pl in
  let num_store_remaining = num_remaining_store pl in
  let num_available = num_remaining_available pl in
  let sun_dir = Game.sun_dir new_game in
  let hlo =
    let plnt_opt =
      s.current_position |> Game.cell_at new_game |> Cell.plant
    in
    match plnt_opt with
    | None -> None
    | Some plant ->
        if Plant.player_id plant <> pl_id then None
        else
          let plnt_stg = plant |> Plant.plant_stage in
          if Player.is_in_available plnt_stg pl then
            Some (false, plnt_stg)
          else Some (true, plnt_stg)
  in
  let new_gui =
    Gui.update_turn pl_id
      (Player.light_points pl)
      (Player.score_points pl)
      num_store_remaining num_available hlo s.gui
    |> Gui.update_sun sun_dir
  in
  let new_state = { s with game = new_game; gui = new_gui } in
  let new2_gui =
    Gui.update_message
      (update_message new_state new_state.current_position)
      ANSITerminal.White new_gui
  in
  let new2_state = { s with game = new_game; gui = new2_gui } in
  new2_state

let plant s =
  try
    if
      Game.can_plant_seed s.current_position
        (Game.player_of_turn s.game |> Player.player_id)
        s.game
    then
      plant_helper s
        (Game.plant_seed
           (Game.player_of_turn s.game |> Player.player_id))
    else if Game.can_plant_small s.current_position s.game then
      plant_helper s Game.plant_small |> end_turn
    else if Game.can_grow_plant s.current_position s.game then
      let plnt_stg =
        Game.cell_at s.game s.current_position
        |> Cell.plant |> extract_plant |> Plant.plant_stage
      in
      match plnt_stg with
      | Seed -> plant_helper s Game.grow_plant
      | Small -> plant_helper s Game.grow_plant
      | Medium -> plant_helper s Game.grow_plant
      | Large -> failwith "Should Not Happen"
    else if Game.can_harvest s.current_position s.game then
      plant_helper s Game.harvest
    else s
  with
  | Board.IllegalPlacePlant ->
      let new_gui =
        Gui.update_message "Illegal Placement of Plant" ANSITerminal.Red
          s.gui
      in
      { s with gui = new_gui }
  | PlantInventory.OutOfPlant Plant.Seed ->
      plant_helper_exn s
        (Game.plant_seed
           (Game.player_of_turn s.game |> Player.player_id))
        Plant.Seed
  | PlantInventory.OutOfPlant Plant.Small ->
      if Game.is_setup s.game then end_turn s
      else plant_helper_exn s Game.grow_plant Plant.Small
  | PlantInventory.OutOfPlant Plant.Medium ->
      plant_helper_exn s Game.grow_plant Plant.Medium
  | PlantInventory.OutOfPlant Plant.Large ->
      plant_helper_exn s Game.grow_plant Plant.Large

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
  | 'x' -> raise End
  | _ -> s

let rec read_char (s : t) =
  Graphics.open_graph " 100x100+900+0";
  while true do
    try
      let a = Graphics.wait_next_event [ Graphics.Key_pressed ] in
      if a.Graphics.keypressed then (
        let new_state = handle_char s a.Graphics.key in
        render new_state.gui;
        read_char new_state)
      else read_char s
    with End -> Graphics.close_graph ()
  done
