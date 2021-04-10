(** The abstract type representing the current state of the user
    interface. *)
type t

(** [init_state gui] is a state with a current_position at col = 0 and
    diag = 0 and the starting map *)
val init_state : Gui.t -> t

(** [scroll] returns a new state of the neighbor of the original
    current_postion in direction [d] *)
val scroll : t -> HexUtil.dir -> t

(** [handle_char] updates the current state's current position based on
    the char inputed. [handle_char] uses the WASD keys to move the
    current position Up, Left, Down, Right, respectively *)
val handle_char : t -> char -> t

(** [read_char] takes in the current state and will read the key press
    and send the character to the [handle_char] function to
    appropriately update the current state*)
val read_char : t -> unit
