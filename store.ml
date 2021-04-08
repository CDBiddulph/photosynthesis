type t = unit

exception InsufficientLightPoints of int

exception FullOfPlant of Plant.plant_stage

let init_store = ()

let buy_plant store stage light_points = failwith "Not Implemented"

let add_plant store stage = failwith "Not Implemented"

let cost store stage = failwith "Not Implemented"

let cost_at_n store stage n = failwith "Not Implemented"

let num_remaining store stage = failwith "Not Implemented"

let capacity store stage = failwith "Not Implemented"
