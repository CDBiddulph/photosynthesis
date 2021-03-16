type t

exception InvalidPlacement

(* this function will probably only be used by GUI*)
val is_place_plant_legal : t -> Cell.t -> Plant.t -> bool

val place_plant : t -> Cell.t -> Plant.t -> t

val end_turn : t -> t

val sun_dir : t -> HexMap.dir
