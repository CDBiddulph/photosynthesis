(** The abstract type representing the game's board. *)
type t

type ruleset =
  | Normal
  | Extended

(** Raised when an invalid plant placement is attempted. *)
exception InvalidPlantPlacement

(** Raised when an invalid harvest is attempted. *)
exception InvalidHarvest

(** [init_game players map sun] initializes a game with the given
    players, board, and sun direction. The first player in the list is
    assumed to be the first, and the order of play follows the order of
    the players in the list. Requires: [players] is non-empty *)
val init_game : Player.t list -> HexMap.t -> HexUtil.dir -> t

(** [is_place_plant_legal board coord plant] returns [true] iff placing
    [plant] at [coord] on [board] is a legal move. *)
val is_place_plant_legal : t -> HexUtil.coord -> Plant.t -> bool

(** [place_plant board coord plant] places [plant] at [coord] on
    [board]. Raises: [InvalidPlantPlacement] if placing [plant] in
    [coord] on [board] is not a legal move. *)
val place_plant : t -> HexUtil.coord -> Plant.t -> t

(** [harvest board player coord] removes the plant at [coord] on
    [board]. Raises: [InvalidPlantPlacement] if placing [plant] in
    [coord] on [board] with [player] is not a legal move. *)
val harvest : t -> Player.player_id -> HexUtil.coord -> t

val end_turn : t -> t

val sun_dir : t -> HexUtil.dir

(* You'll want to use HexMap.flatten for this. *)

(** [cells board] is a list of the Cells in [board], in any order. *)
val cells : t -> Cell.t list
