type coord = {
  col : int;
  diag : int;
}

type dir = int

let num_dirs = 6

let move_cw dir = if dir = 0 then num_dirs - 1 else dir - 1
