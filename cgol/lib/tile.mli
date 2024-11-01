type state =
  | Dead
  | Alive

type t =
  { xpos : int
  ; ypos : int
  ; size : int
  ; state : state
  }

(** [create xpos ypos size state] returns a Tile.t set with the provided arguments. *)
val create : int -> int -> int -> state -> t

(** [new_state state tile] Returns the contents of [tile] but with the provided [state]. *)
val new_state : state -> t -> t

(** [to_string tile] Returns [tile] represented as a String.t *)
val to_string : t -> String.t

(** [draw tile] Uses Raylib to draw the [tile] to the screen.*)
val draw : t -> unit
