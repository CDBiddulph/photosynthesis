(** Utility module for player id type needed across other modules. *)

(** Type representing a player id. *)
type t = int

(** Value of the first player's id. *)
val first : t

(** [generate_player_ids num_players] is a list of [num_players] unique
    ordered player ids. *)
val generate_player_ids : int -> t list
