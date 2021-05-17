(** The UI handles user input, updating the game and conveying changes
    in the game to the GUI. It also handles the position of the player's
    cursor, and independently keeps track of whether a winner has been
    declared, whether photosynthesis is occurring, and whether the
    instructions are being displayed. *)

(** The abstract type representing the current state of the user
    interface. *)
type t

(** [init_state instr gui game] is a state with its cursor at
    init_cursor, the instructions displayed if [instr], [gui], and
    [game]. *)
val init_state : bool -> Gui.t -> Game.t -> t

(** [init_cursor] is the starting location of the cursor in a new state,
    specifically [{ col = 0; diag = 0; }]. *)
val init_cursor : HexUtil.coord

(** [scroll dir state] returns a new state like [state] but with the
    current position moved one step in direction [dir] *)
val scroll : HexUtil.dir -> t -> t

(** [handle_char] updates the current state's current position based on
    the char inputed. [handle_char] uses the WASD keys to move the
    current position Up, Left, Down, Right, respectively *)
val handle_char : t -> char -> t

(** [read_char] takes in the current state and will read the key press
    and send the character to the [handle_char] function to
    appropriately update the current state*)
val read_char : t -> unit
