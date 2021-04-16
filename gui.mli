(** Takes data for various rendered components and re-renders them on
    command. *)

(** Type representing the GUI state *)
type t

(** [init_gui cells player_params] is the GUI for a board of cells,
    constructed from the ground up. A hexagon will be created in every
    place corresponding to a Some value in [cells]. Then,
    [update_cells cells] will be called. The colors and chars of
    [player_params] will be used to render the trees of each individual
    player. *)
val init_gui :
  Cell.t list -> (PlayerId.t * (char * ANSITerminal.color)) list -> t

(** [update_cells cells gui] is [gui] with the contents of each cell in
    [cells] updated. If [Cell.plant c = None] for some [c] in [cells],
    the space corresponding to [c] will display a marker showing the
    type of soil in [c]. Otherwise, if [Cell.plant c = Some p], [p] will
    be displayed. Precondition: hexagons of board match the hexagons
    formed in init *)
val update_cells : Cell.t list -> t -> t

(** [update_sun gui dir] updates the GUI so that the rays of the sun
    will appear to point in direction [dir].*)
val update_sun : HexUtil.dir -> t -> t

(** [update_cursor gui color coord_opt], if [coord_opt = Some coord],
    updates the GUI to create a cursor with color [color] at position
    [coord]. If [coord_opt = None], no cursor is drawn. If there was
    previously a cursor on display, it is removed. *)
val update_cursor : ANSITerminal.color -> HexUtil.coord option -> t -> t

(* erase the previous render and print the new render to the screen
   based on the state in t *)
val render : t -> unit
