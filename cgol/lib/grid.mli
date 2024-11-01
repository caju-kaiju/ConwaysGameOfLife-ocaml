type 'a t =
  { nrows : int
  ; ncols : int
  ; elements : 'a list
  }

type 'a element =
  { row : int
  ; col : int
  ; element : 'a
  }

(** [create row col list] represents [list] as a [Grid]. The length of [list] must be equal to [row] * [col].
    If not equal, then an assert is thrown. *)
val create : int -> int -> 'a list -> 'a t

(** [new_element element item] copies the content of element but replaces the item stored within element with
    [item]. *)
val new_element : 'a element -> 'a -> 'a element

(** [calc_index grid row col] calculates the list index of given the [row] and [col] representation of
    of the index. *)
val calc_index : 'a t -> int -> int -> int

(** [calc_row grid index] calculates the row number the [index] resides within [grid]. *)
val calc_row : 'a t -> int -> int

(** [calc_col grid index row] calculates the col number the [index] resides within [grid]. *)
val calc_col : 'a t -> int -> int -> int

(** [get row col grid] returns the item at [row] and [col].*)
val get : int -> int -> 'a t -> 'a

(** [set row col item grid] returns a new Grid with [item] at [row] and [col] within [grid].*)
val set : int -> int -> 'a -> 'a t -> 'a t

(** [set_many list grid] Takes a [list] of (row, col, item) tuples and sets the position of row and col
    to the corresponding item in [grid]*)
val set_many : (int * int * 'a) list -> 'a t -> 'a t

(** [iter f grid] iterates over [grid], applying [f] to each index.*)
val iter : ('a -> unit) -> 'a t -> unit

(** [find_all f grid] iterates through all items in [grid], returning a list of all items that meet the
    condition of [f].*)
val find_all : ('a -> bool) -> 'a t -> 'a element list

(** [get_neighbors row col grid] returns a list of all adjacent items to [row] and [col]. *)
val get_neighbors : int -> int -> 'a t -> 'a element list
