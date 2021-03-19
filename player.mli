type t

(** [player_id] is a unique integer representing a player. *)
type player_id = int

(** [player_id player] is the unique [player_id] of [player]. *)
val player_id : t -> player_id

(** [light_points player] is the number of light points that [player]
    has. *)
val light_points : t -> int
