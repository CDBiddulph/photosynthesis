(* open Plant *)

type soil = int

type t = {
  soil : soil;
  plant : Plant.t option;
  location : HexUtil.coord;
}

let init_cell s p l = { soil = s; plant = p; location = l }

let coord c = c.location

let soil c = c.soil

let plant c = c.plant

let set_plant cell plant_opt = { cell with plant = plant_opt }
