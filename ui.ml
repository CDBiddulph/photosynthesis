open Cell
open Plant
open HexMap
open HexUtil
open Board
open Gui
open Graphics
open Game
open Player

exception Invalid_Direction

exception Invalid_Cell

type t = {
  current_position : HexUtil.coord;
  instr : bool;
  game : Game.t;
  gui : Gui.t;
}

let init_cursor = { col = 0; diag = 0 }

let init_state (instr : bool) (gui : Gui.t) (game : Game.t) : t =
  { current_position = init_cursor; instr; game; gui }

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
  let pl = Game.player_of_turn s.game in
  if Game.can_plant_seed coord s.game then
    "(P) Plant Seed for "
    ^ string_of_int (Player.cost_to_buy_and_grow Seed pl)
    ^ " light point(s)"
  else if Game.can_plant_small coord s.game then "(P) Plant Small Tree"
  else if Game.can_grow_plant coord s.game then
    let plnt_stg =
      Game.cell_at s.game coord
      |> Cell.plant |> extract_plant |> Plant.plant_stage
    in
    match plnt_stg with
    | Seed ->
        "(P) Grow Small Tree for "
        ^ string_of_int (Player.cost_to_buy_and_grow Small pl)
        ^ " light point(s)"
    | Small ->
        "(P) Grow Medium Tree for "
        ^ string_of_int (Player.cost_to_buy_and_grow Medium pl)
        ^ " light point(s)"
    | Medium ->
        "(P) Grow Tall Tree for "
        ^ string_of_int (Player.cost_to_buy_and_grow Large pl)
        ^ " light point(s)"
    | Large -> failwith "Should Not Happen"
  else if Game.can_harvest coord s.game then
    "(P) Harvest Tall Tree for 4 light points"
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
  let new_state = { s with game = new_game } in
  let new_gui =
    let cells = Game.cells new_game in
    Gui.update_cells cells new_state.gui
    |> Gui.update_message
         (update_message { s with game = new_game } s.current_position)
         ANSITerminal.White
    |> Gui.update_available num_available
    |> Gui.update_store_remaining num_store_remaining
    |> Gui.update_player_lp (Player.light_points pl)
    |> Gui.update_player_sp (Player.score_points pl)
    |> Gui.update_plant_highlight hlo
  in
  let newer_state = { s with gui = new_gui; game = new_game } in
  newer_state

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

let out_of_plant_exn s plnt_stg =
  let str = "Out of " ^ Plant.string_of_plant_stage plnt_stg in
  let new_gui = Gui.update_message str ANSITerminal.Red s.gui in
  { s with gui = new_gui }

let plant s =
  try
    if Game.can_plant_seed s.current_position s.game then
      plant_helper s Game.buy_and_grow_plant
    else if Game.can_plant_small s.current_position s.game then
      plant_helper s Game.plant_small |> end_turn
    else if Game.can_grow_plant s.current_position s.game then
      plant_helper s Game.buy_and_grow_plant
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
      out_of_plant_exn s Plant.Seed
  | PlantInventory.OutOfPlant Plant.Small ->
      if Game.is_setup s.game then end_turn s
      else out_of_plant_exn s Plant.Small
  | PlantInventory.OutOfPlant Plant.Medium ->
      out_of_plant_exn s Plant.Medium
  | PlantInventory.OutOfPlant Plant.Large ->
      out_of_plant_exn s Plant.Large
  | Store.InsufficientLightPoints cost ->
      let new_gui =
        Gui.update_message
          ("Action requires " ^ string_of_int cost ^ " light points")
          ANSITerminal.Red s.gui
      in
      { s with gui = new_gui }

let buy_helper s num =
  match num with
  | 1 -> Game.buy_plant Plant.Seed s.game
  | 2 -> Game.buy_plant Plant.Small s.game
  | 3 -> Game.buy_plant Plant.Medium s.game
  | 4 -> Game.buy_plant Plant.Large s.game
  | _ -> failwith "Should Not Happen"

let buy s num =
  try
    let new_game = buy_helper s num in
    let pl = Game.player_of_turn new_game in
    let num_store_remaining = num_remaining_store pl in
    let num_available = num_remaining_available pl in
    let new_gui =
      let cells = Game.cells new_game in
      Gui.update_cells cells s.gui
      |> Gui.update_message "" ANSITerminal.White
      |> Gui.update_available num_available
      |> Gui.update_store_remaining num_store_remaining
      |> Gui.update_player_lp (Player.light_points pl)
      |> Gui.update_player_sp (Player.score_points pl)
    in
    let new_state = { s with gui = new_gui; game = new_game } in
    new_state
  with
  | Store.InsufficientLightPoints cost ->
      let new_gui =
        Gui.update_message
          ("Action requires " ^ string_of_int cost ^ " light points")
          ANSITerminal.Red s.gui
      in
      { s with gui = new_gui }
  | PlantInventory.OutOfPlant Plant.Seed ->
      out_of_plant_exn s Plant.Seed
  | PlantInventory.OutOfPlant Plant.Small ->
      out_of_plant_exn s Plant.Small
  | PlantInventory.OutOfPlant Plant.Medium ->
      out_of_plant_exn s Plant.Medium
  | PlantInventory.OutOfPlant Plant.Large ->
      out_of_plant_exn s Plant.Large

let toggle_instructions s =
  let new_instr = not s.instr in
  {
    s with
    gui = update_instructions new_instr s.gui;
    instr = new_instr;
  }

exception End

exception Invalid_Key

let handle_char s c =
  match c with
  | 'i' -> toggle_instructions s
  | 'x' -> raise End
  | _ -> (
      if s.instr then raise Invalid_Key
      else
        match c with
        | 'q' -> scroll s 3
        | 'e' -> scroll s 5
        | 'w' -> scroll s 4
        | 'a' -> scroll s 2
        | 's' -> scroll s 1
        | 'd' -> scroll s 0
        | 'p' -> plant s
        | '1' -> buy s 1
        | '2' -> buy s 2
        | '3' -> buy s 3
        | '4' -> buy s 4
        | 'f' ->
            if Game.is_setup s.game then raise Invalid_Key
            else end_turn s
        | _ -> raise Invalid_Key)

let rec read_char (s : t) =
  Graphics.open_graph " 100x100+900+0";
  while true do
    try
      let a = Graphics.wait_next_event [ Graphics.Key_pressed ] in
      let new_state = handle_char s a.Graphics.key in
      render new_state.gui;
      read_char new_state
    with
    | Invalid_Key -> read_char s
    | End -> Graphics.close_graph ()
  done
