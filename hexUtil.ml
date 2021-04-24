type coord = {
  col : int;
  diag : int;
}

type dir = int

let num_dirs = 6

let move_cw dir = (dir + 1) mod num_dirs

let dir_of_int n = n mod num_dirs
