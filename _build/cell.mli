type t

type soil_type = int

val coord : HexMap.coord

val soil_type : t -> soil_type

val plant : t -> Plant.t option
