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

let update_message (s : t) (coord : HexUtil.coord) =
  if Game.can_plant_seed coord s.game then "(P) Plant Seed"
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
        |> Gui.update_message (update_message s pos) ANSITerminal.White
        |> Gui.update_plant_highlight (Game.plant_hl_loc pos s.game)
      in
      let new_state =
        { s with current_position = pos; gui = new_gui }
      in
      new_state

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
         (update_message new_state new_state.current_position)
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

(** [end_turn s] will move the game to the next player's turn and set
    [photosynthesis = false], except when doing so will cause
    photosynthesis to occur and [s.photosynthesis] is already false. In
    that case, it will just change the GUI to display photosynthesis and
    set [s.photosynthesis = true]. *)
let end_turn s =
  if Game.will_photo s.game && not s.photosynthesis then
    let lp, sun_dir = Game.photo_preview s.game in
    let new_gui =
      s.gui |> Gui.photosynthesis lp |> Gui.update_sun sun_dir
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
    | None ->
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
          |>
          if s.photosynthesis then Gui.clear_photosynthesis else Fun.id
        in
        let new_state =
          {
            s with
            game = new_game;
            gui = new_gui;
            photosynthesis = false;
          }
        in
        let new_gui' =
          Gui.update_message
            (update_message new_state new_state.current_position)
            ANSITerminal.White new_gui
        in
        { new_state with gui = new_gui' }

let out_of_plant_exn s plnt_stg =
  let str = "Out of " ^ Plant.string_of_plant_stage plnt_stg in
  let new_gui = Gui.update_message str ANSITerminal.Red s.gui in
  { s with gui = new_gui }

let try_action f s =
  try f s with
  | Board.IllegalPlacePlant ->
      failwith "I don't think this should happen"
      (* let new_gui = Gui.update_message "Illegal Placement of Plant"
         ANSITerminal.Red s.gui in { s with gui = new_gui } *)
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
      (* TODO: This update_message seems wrong *)
      |> Gui.update_message "" ANSITerminal.White
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

let handle_char s c =
  if s.ended then raise End
  else if s.photosynthesis then end_turn s
  else
    match c with
    | 'i' -> toggle_instructions s
    | 'x' -> raise End
    | _ -> (
        if s.instr then raise Invalid_Key
        else
          match c with
          | 'q' -> scroll 3 s
          | 'e' -> scroll 5 s
          | 'w' -> scroll 4 s
          | 'a' -> scroll 2 s
          | 's' -> scroll 1 s
          | 'd' -> scroll 0 s
          | 'p' -> plant s
          | '1' -> buy Seed s
          | '2' -> buy Small s
          | '3' -> buy Medium s
          | '4' -> buy Large s
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
