(** Takes data for various rendered components and re-renders them on
    command. *)
open ANSITerminal

(** Type representing the GUI state *)
type t

(** [init_gui store_costs init_available init_next_sp cells player_params]
    is the GUI for a board of cells, constructed from the ground up. A
    hexagon will be created in every place corresponding to a Some value
    in [cells]. Then, [update_cells cells] will be called. The colors
    and chars of [player_params] will be used to render the trees of
    each individual player. The store and available area, when they are
    updated, will render according to the player id that equals 1 in
    [player_params]. The store will have capacities according to length
    of each list in [store_costs], in order of [Plant.all_stages]. The
    cost of each plant in the store will correspond to the costs in
    [store_costs]. The available area will have [init_available] numbers
    of plants in each row, in order of [Plant.all_stages]. The displayed
    scoring points will be initialized to [init_next_sp], which should
    be given in the order of the soil types 1, 2, 3, 4, but will be
    displayed in the order of 4, 3, 2, 1. Precondition: for all values
    [sp] in [init_next_sp], [sp >= 0 && sp < 100] *)
val init_gui :
  int list list ->
  int list ->
  int list ->
  Cell.t list ->
  (PlayerId.t * (char * color)) list ->
  t

(** [update_cell cell gui] is gui with the contents of [cell] updated.
    If [Cell.plant c = None] for [cell], the space corresponding to
    [cell] will display a marker showing the type of soil in [cell].
    Otherwise, if [Cell.plant cell = Some p], [p] will be displayed.
    Precondition: hexagons of board match the hexagons formed in init*)
val update_cell : Cell.t -> t -> t

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

(** [update_cursor coord_opt gui], if [coord_opt = Some coord], updates
    the GUI to create a red cursor at position [coord]. If
    [coord_opt = None], no cursor is drawn. If there was previously a
    cursor displayed, it is removed. *)
val update_cursor : HexUtil.coord option -> t -> t

(** [update_message text color gui] updates the message at the top of
    [gui] with [text] and [color]. *)
val update_message : string -> color -> t -> t

(** [update_turn lp sp player_id num_store_remaining num_available highlight_loc_opt gui]
    configures [gui] to render its store and available area according to
    [player_id]; performs [update_player_lp lp], [update_player_sp sp]
    [update_bought num_store_remaining],
    [update_available num_available], and
    [update_plant_highlight highlight_loc_opt] on [gui]; and updates the
    sign displaying whose turn it is. *)
val update_turn :
  PlayerId.t ->
  int ->
  int ->
  int list ->
  int list ->
  (bool * Plant.plant_stage) option ->
  t ->
  t

(** [update_store_remaining num_remaining gui] is [gui] with the number
    of plants colored according to the current player's turn as stored
    by [gui] in each row updated from top to bottom according to
    [num_remaining], in the order of [Plant.all_stages], and the rest of
    the plants colored magenta. Requires:
    [List.length num_remaining = List.length Plant.all_stages]. *)
val update_store_remaining : int list -> t -> t

(** [update_available num_available gui] is [gui] with the number of
    plants in each row in the available area updated from top to bottom
    according to [num_available], in the order of [Plant.all_stages]. *)
val update_available : int list -> t -> t

(** [update_plant_highlight loc_opt gui] removes any plant highlights
    from gui if [loc_opt = None]. Otherwise, if [loc = is_store, stage]:
    if [is_store = true], the first unbought plant of [stage] in the
    store from the left is highlighted in white; else, the first plant
    of [stage] in the available area from the left is highlighted in
    white. If the plant to highlight does not exist, removes any plant
    highlights from [gui], as if [loc_opt = None]. *)
val update_plant_highlight : (bool * Plant.plant_stage) option -> t -> t

(** [update_cell_highlight coords gui] is gui with all of its existing
    highlights cleared, then all of the cells at each coordinate in
    [coords] highlighted in green. If [coords = \[\]], there will be no
    highlighted cells. *)
val update_cell_highlight : HexUtil.coord list -> t -> t

(** [update_next_sp soil sp gui] is [gui] with the scoring points
    displayed next to [soil] overwritten by [sp]. Precondition:
    [sp >= 0 && sp < 100]*)
val update_next_sp : Cell.soil -> int -> t -> t

(** [update_player_lp lp gui] is [gui] with the light points of the
    current player overwritten with [lp]. *)
val update_player_lp : int -> t -> t

(** [update_player_sp sp gui] is [gui] with the scoring points of the
    current player overwritten with [sp]. *)
val update_player_sp : int -> t -> t

(** [photosynthesis lp gui] is [gui] with numbers under each of the
    trees at the coordinates in the color of the proper player id -
    these should be the number of light points each of these trees gains
    in this stage of photosynthesis. *)
val photosynthesis :
  (PlayerId.t * (HexUtil.coord * int) list) list -> t -> t

(** [photosynthesis gui] is [gui] with any numbers drawn in
    [photosynthesis] cleared. *)
val clear_photosynthesis : t -> t

(* erase the previous render and print the new render to the screen
   based on the state in t *)
val render : t -> unit
