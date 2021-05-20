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
  ended : bool;
  photosynthesis : bool;
  game : Game.t;
  gui : Gui.t;
}

let init_cursor = { col = 0; diag = 0 }

let init_state (instr : bool) (gui : Gui.t) (game : Game.t) : t =
  {
    current_position = init_cursor;
    instr;
    ended = false;
    photosynthesis = false;
    game;
    gui;
  }

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

(** [buy_grow_cost_strs to_stage player] is a pair
    [(actions_str, sum_str)]. [actions_str] is in \["Buy and Grow"; "Buy
    and Plant"; "Buy"; "Plant"\] and reflects the single set of actions
    [player] could take to result in putting [to_stage] on the board.
    [sum_str] is either in the form ["{int}"] or
    ["{int} + {int} = {int}"] and reflects the light points that would
    be used for the actions in [actions_str] in order, and their sum if
    the number of actions > 1. *)
let buy_grow_cost_strs to_stage player =
  let buy_cost = Player.cost_to_buy to_stage player in
  let buy_grow_cost = Player.cost_to_buy_and_grow to_stage player in
  let grow_cost, action2_str =
    if to_stage = Seed then (Player.cost_to_plant_seed, "Plant")
    else (Player.cost_to_grow to_stage, "Grow")
  in
  (* under the assumption that buying never costs 0 LP, if we wish to
     show that buying was not performed, it should be sufficient to show
     that grow_cost = buy_grow_cost *)
  let actions_str, sum_str =
    if grow_cost = buy_grow_cost then (
      assert (grow_cost = buy_grow_cost);
      (action2_str, string_of_int grow_cost))
    else (
      assert (buy_cost + grow_cost = buy_grow_cost);
      ( "Buy and " ^ action2_str,
        string_of_int buy_cost ^ " + " ^ string_of_int grow_cost ^ " = "
        ^ string_of_int buy_grow_cost ))
  in
  (actions_str, sum_str)

let message_at_current_pos s =
  let pl = Game.player_of_turn s.game in
  let coord = s.current_position in
  if Game.can_plant_seed coord s.game then
    let actions_str, sum_str = buy_grow_cost_strs Seed pl in
    "(P) " ^ actions_str ^ " Seed for " ^ sum_str ^ " LP"
  else if Game.can_plant_small coord s.game then "(P) Plant Small"
  else if Game.can_grow_plant coord s.game then
    let stage =
      Game.cell_at s.game coord
      |> Cell.plant |> extract_plant |> Plant.plant_stage |> next_stage
    in
    let stage_str =
      (stage |> string_of_plant_stage |> String.capitalize_ascii)
      ^ if stage = Seed then "" else " Tree"
    in
    let actions_str, sum_str = buy_grow_cost_strs stage pl in
    "(P) " ^ actions_str ^ " to " ^ stage_str ^ " for " ^ sum_str
    ^ " LP"
  else if Game.can_harvest coord s.game then
    "(P) Harvest Large Tree for "
    ^ string_of_int cost_to_harvest
    ^ " LP"
  else ""

let scroll d s =
  let map = Game.board s.game |> Board.map in
  let new_pos = HexMap.neighbor map s.current_position d in
  match new_pos with
  | None ->
      let new_gui =
        Gui.update_message "Invalid Direction" ANSITerminal.Red s.gui
      in
      let new_state = { s with gui = new_gui } in
      new_state
  | Some pos ->
      let new_gui =
        Gui.update_cursor new_pos s.gui
        |> Gui.update_message
             (message_at_current_pos { s with current_position = pos })
             ANSITerminal.White
        |> Gui.update_plant_highlight (Game.plant_hl_loc pos s.game)
      in
      { s with current_position = pos; gui = new_gui }

let plant_helper s f =
  let new_game = f s.current_position s.game in
  let pl = Game.player_of_turn new_game in
  let num_store_remaining = num_remaining_store pl in
  let num_available = num_remaining_available pl in
  let new_state = { s with game = new_game } in
  let new_gui =
    let cells = Game.cells new_game in
    Gui.update_cells cells new_state.gui
    |> Gui.update_message
         (message_at_current_pos { s with game = new_game })
         ANSITerminal.White
    |> Gui.update_available num_available
    |> Gui.update_store_remaining num_store_remaining
    |> Gui.update_player_lp (Player.light_points pl)
    |> Gui.update_player_sp (Player.score_points pl)
    |> Gui.update_plant_highlight
         (Game.plant_hl_loc new_state.current_position new_state.game)
  in
  let newer_state = { s with gui = new_gui; game = new_game } in
  newer_state

let end_turn_no_winners_or_photo s =
  let pl = Game.player_of_turn s.game in
  let pl_id = Player.player_id pl in
  let num_store_remaining = num_remaining_store pl in
  let num_available = num_remaining_available pl in
  let new_gui =
    s.gui
    |> Gui.update_turn pl_id
         (Player.light_points pl)
         (Player.score_points pl)
         num_store_remaining num_available
         (Game.plant_hl_loc s.current_position s.game)
    |> Gui.update_message (message_at_current_pos s) ANSITerminal.White
    |> if s.photosynthesis then Gui.clear_photosynthesis else Fun.id
  in
  { s with gui = new_gui; photosynthesis = false }

(** [end_turn s] will move the game to the next player's turn and set
    [photosynthesis = false], except when doing so will cause
    photosynthesis to occur and [s.photosynthesis] is already false. In
    that case, it will just change the GUI to display photosynthesis and
    set [s.photosynthesis = true]. *)
let end_turn s =
  if Game.will_photo s.game && not s.photosynthesis then
    let lp, sun_dir, sun_rev = Game.photo_preview s.game in
    let new_gui =
      s.gui |> Gui.photosynthesis lp |> Gui.update_sun sun_dir
      |> Gui.update_sun_revolution sun_rev
      |> Gui.update_message "PHOTOSYNTHESIS - press any key to continue"
           Yellow
    in
    { s with gui = new_gui; photosynthesis = true }
  else
    let new_game = Game.end_turn s.game in
    match Game.winners new_game with
    | Some ws ->
        {
          s with
          game = new_game;
          gui = s.gui |> update_end_screen ws;
          ended = true;
          photosynthesis = false;
        }
    | None -> end_turn_no_winners_or_photo { s with game = new_game }

let out_of_plant_exn s plnt_stg =
  let str = "Out of " ^ Plant.string_of_plant_stage plnt_stg in
  let new_gui = Gui.update_message str ANSITerminal.Red s.gui in
  { s with gui = new_gui }

let try_action f s =
  try f s with
  | Board.IllegalPlacePlant -> failwith "Illegal Place Plant"
  | PlantInventory.OutOfPlant Plant.Seed ->
      out_of_plant_exn s Plant.Seed
  | PlantInventory.OutOfPlant Plant.Small ->
      out_of_plant_exn s Plant.Small
  | PlantInventory.OutOfPlant Plant.Medium ->
      out_of_plant_exn s Plant.Medium
  | PlantInventory.OutOfPlant Plant.Large ->
      out_of_plant_exn s Plant.Large
  | Store.InsufficientLightPoints cost ->
      let new_gui =
        Gui.update_message
          ("Action requires " ^ string_of_int cost ^ " LP")
          ANSITerminal.Red s.gui
      in
      { s with gui = new_gui }

let plant state =
  let plant_f s =
    if Game.can_plant_seed s.current_position s.game then
      plant_helper s Game.buy_and_grow_plant
    else if Game.can_plant_small s.current_position s.game then
      plant_helper s Game.plant_small |> end_turn
    else if Game.can_grow_plant s.current_position s.game then
      plant_helper s Game.buy_and_grow_plant
    else if Game.can_harvest s.current_position s.game then
      plant_helper s Game.harvest
    else s
  in
  try_action plant_f state

let buy stage state =
  let buy_f s =
    let new_game = Game.buy_plant stage s.game in
    let pl = Game.player_of_turn new_game in
    let num_store_remaining = num_remaining_store pl in
    let num_available = num_remaining_available pl in
    let new_gui =
      let cells = Game.cells new_game in
      Gui.update_cells cells s.gui
      |> Gui.update_message
           (message_at_current_pos { s with game = new_game })
           ANSITerminal.White
      |> Gui.update_available num_available
      |> Gui.update_store_remaining num_store_remaining
      |> Gui.update_player_lp (Player.light_points pl)
      |> Gui.update_player_sp (Player.score_points pl)
    in
    { s with gui = new_gui; game = new_game }
  in
  try_action buy_f state

let toggle_instructions s =
  let new_instr = not s.instr in
  {
    s with
    gui = update_instructions new_instr s.gui;
    instr = new_instr;
  }

exception End

exception Invalid_Key

let handle_char_no_setup s c =
  match c with
  | '1' -> buy Seed s
  | '2' -> buy Small s
  | '3' -> buy Medium s
  | '4' -> buy Large s
  | 'f' -> end_turn s
  | _ -> raise Invalid_Key

let handle_char_no_instr s c =
  match c with
  | 'q' -> scroll 3 s
  | 'e' -> scroll 5 s
  | 'w' -> scroll 4 s
  | 'a' -> scroll 2 s
  | 's' -> scroll 1 s
  | 'd' -> scroll 0 s
  | 'p' -> plant s
  | _ ->
      if Game.is_setup s.game then raise Invalid_Key
      else handle_char_no_setup s c

let handle_char_no_end_or_photo s c =
  match c with
  | 'i' -> toggle_instructions s
  | 'x' -> raise End
  | _ -> if s.instr then raise Invalid_Key else handle_char_no_instr s c

let handle_char s c =
  if s.ended then raise End
  else if s.photosynthesis then end_turn s
  else handle_char_no_end_or_photo s c

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
    | End -> exit 0
  done
