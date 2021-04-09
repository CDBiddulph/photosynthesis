type ruleset =
  | Normal
  | Extended

(** The type [round_phase] represents the part of the round. *)
type round_phase =
  | Photosynthesis
  | Life_Cycle

(** A record type representing a game and its data. [sun_dir] indicates
    the direction that shadows are cast. *)
type t = {
  map : HexMap.t;
  sun_dir : HexUtil.dir;
  rules : ruleset;
}

exception InvalidPlantPlacement

exception InvalidHarvest

let init_board ruleset =
  { map = HexMap.init_map (); sun_dir = 0; rules = ruleset }

let is_place_plant_legal board coord player stage =
  HexMap.valid_coord board.map coord
  &&
  match HexMap.cell_at board.map coord with
  | None -> false
  | Some cell -> (
      match Cell.plant cell with
      | None -> true
      | Some old_plt -> (
          let old_player = Plant.player_id old_plt in
          old_player = player
          &&
          match Plant.plant_stage old_plt with
          | Plant.Seed -> stage = Plant.Small
          | Plant.Small -> stage = Plant.Medium
          | Plant.Medium -> stage = Plant.Large
          | Plant.Large -> false))

let place_plant board coord plant =
  if
    is_place_plant_legal board coord (Plant.player_id plant)
      (Plant.plant_stage plant)
  then
    match HexMap.cell_at board.map coord with
    | None -> failwith "should be a valid cell"
    | Some cell ->
        {
          board with
          map =
            HexMap.set_cell board.map
              (Some (Cell.init_cell (Cell.soil cell) (Some plant) coord))
              coord;
        }
  else raise InvalidPlantPlacement

(** [shadows map c1 c2] determines if the [Plant.t] in [c1] would shadow
    the [Plant.t] in [c2] based on size and distance. *)
let shadows map c1 c2 =
  let open Cell in
  let open Plant in
  let c1_cell_opt = HexMap.cell_at map c1 in
  let c2_cell_opt = HexMap.cell_at map c2 in
  match c1_cell_opt with
  | None -> false
  | Some cell_1 -> (
      match c2_cell_opt with
      | None -> true
      | Some cell_2 -> (
          let c1_plnt_opt = plant cell_1 in
          let c2_plnt_opt = plant cell_2 in
          match c2_plnt_opt with
          | None -> true
          | Some c2_plt -> (
              let c2_plnt = plant_stage c2_plt in
              c2_plnt = Seed
              ||
              match c1_plnt_opt with
              | None -> false
              | Some c1_plt -> (
                  let c1_plnt = plant_stage c1_plt in
                  match c1_plnt with
                  | Seed -> false
                  | Small ->
                      c2_plnt = Small && HexMap.dist map c1 c2 = 1
                  | Medium ->
                      (c2_plnt = Medium || c2_plnt = Small)
                      && HexMap.dist map c1 c2 <= 2
                  | Large ->
                      (c2_plnt = Large || c2_plnt = Medium
                     || c2_plnt = Small)
                      && HexMap.dist map c1 c2 <= 3))))

(** [lp_map plant] maps [Plant.plant_stage]s to light point amounts. *)
let lp_map (plant : Plant.plant_stage) : int =
  let open Plant in
  match plant with Seed -> 0 | Small -> 1 | Medium -> 2 | Large -> 3

(** [player_lp_helper board player_cells] returns an association list of
    [HexUtil.coord]s where the player's plants are and their respective
    light point values based on [board]'s sun direction.*)
let player_lp_helper (board : t) (player_cells : HexUtil.coord list) :
    (HexUtil.coord * int) list =
  let coord_lp_lst = ref [] in
  for i = 0 to List.length player_cells - 1 do
    let cell_coord = List.nth player_cells i in
    let shadowed =
      let fst_neigh_opt =
        HexMap.neighbor board.map cell_coord board.sun_dir
      in
      match fst_neigh_opt with
      | None -> false
      | Some fst_coord -> (
          let fst_shadow = shadows board.map fst_coord cell_coord in
          let snd_neigh =
            HexMap.neighbor board.map fst_coord board.sun_dir
          in
          match snd_neigh with
          | None -> fst_shadow
          | Some snd_coord -> (
              let snd_shadow =
                fst_shadow || shadows board.map snd_coord cell_coord
              in
              let thd_neigh =
                HexMap.neighbor board.map snd_coord board.sun_dir
              in
              match thd_neigh with
              | None -> snd_shadow
              | Some thd_coord ->
                  snd_shadow || shadows board.map thd_coord cell_coord))
    in
    if not shadowed then
      match HexMap.cell_at board.map cell_coord with
      | None -> failwith "should be a valid cell"
      | Some cell -> (
          let plnt = Cell.plant cell in
          match plnt with
          | None -> ()
          | Some lp_plnt ->
              let lp = lp_map (Plant.plant_stage lp_plnt) in
              coord_lp_lst := (cell_coord, lp) :: !coord_lp_lst)
  done;
  !coord_lp_lst

let get_photo_lp board players =
  let out = ref [] in
  for i = 0 to List.length players - 1 do
    let player = List.nth players i in
    let player_cells =
      List.map
        (fun c -> Cell.coord c)
        (List.filter
           (fun c ->
             match Cell.plant c with
             | None -> false
             | Some plant -> Plant.player_id plant = player)
           (HexMap.flatten board.map))
    in
    out := (player, player_lp_helper board player_cells) :: !out
  done;
  !out

let end_phase (board : t) : t =
  { board with sun_dir = (board.sun_dir + 1) mod 6 }

let move_sun board =
  { board with sun_dir = HexUtil.move_cw board.sun_dir }

let sun_dir board = board.sun_dir

let can_remove board player c =
  match HexMap.cell_at board.map c with
  | None -> false
  | Some cell -> (
      match Cell.plant cell with
      | None -> false
      | Some plnt -> (
          match Plant.plant_stage plnt with
          | Plant.Large -> Plant.player_id plnt = player
          | _ -> false))

let harvest board player_id coord =
  if can_remove board player_id coord then
    match HexMap.cell_at board.map coord with
    | None -> failwith "should be a valid cell"
    | Some cell ->
        let new_cell =
          Some (Cell.init_cell (Cell.soil cell) None coord)
        in
        let new_map = HexMap.set_cell board.map new_cell coord in
        { board with map = new_map }
  else raise InvalidHarvest
(* board *)

let cells (board : t) : Cell.t list = HexMap.flatten board.map

let cell_at board coord = HexMap.cell_at board.map coord
