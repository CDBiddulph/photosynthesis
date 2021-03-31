type coord = {
  col : int;
  diag : int;
}

type dir = int

let init_coord c d = { col = c; diag = d }
