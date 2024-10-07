(*
   Represent a list as a grid. The length of the list must be equal to number of rows * number of columns.
*)

type 'a t =
  { nrows : int
  ; ncols : int
  ; elements : 'a list
  }

let create nrows ncols element_list =
  assert (List.length element_list = nrows * ncols);
  { nrows; ncols; elements = element_list }
;;

let calc_index t row col =
  assert (row >= 0);
  assert (col >= 0);
  assert (row < t.nrows);
  assert (col < t.ncols);
  (row * t.ncols) + col
;;

let get row col t =
  let index = calc_index t row col in
  List.nth t.elements index
;;

let set row col e t =
  let index = calc_index t row col in
  let rec aux elements index acc =
    match elements with
    | [] -> acc
    | h :: t -> if index = 0 then aux [] index (List.rev (e :: acc) @ t) else aux t (index - 1) (h :: acc)
  in
  let new_elements = aux t.elements index [] in
  { nrows = t.nrows; ncols = t.ncols; elements = new_elements }
;;

let iter f t = List.iter f t.elements

let get_neighbors row col t =
  let top_middle =
    if row > 0
    then (
      let trow = row - 1 in
      let tcol = col in
      [ (trow, tcol), get trow tcol t ])
    else []
  in
  let bottom_middle =
    if row < t.nrows - 1
    then (
      let trow = row + 1 in
      let tcol = col in
      [ (trow, tcol), get trow tcol t ])
    else []
  in
  let middle_left =
    if col > 0
    then (
      let trow = row in
      let tcol = col - 1 in
      [ (trow, tcol), get trow tcol t ])
    else []
  in
  let middle_right =
    if col < t.ncols - 1
    then (
      let trow = row in
      let tcol = col + 1 in
      [ (trow, tcol), get trow tcol t ])
    else []
  in
  let top_left =
    if top_middle != [] && middle_left != []
    then (
      let trow = row - 1 in
      let tcol = col - 1 in
      [ (trow, tcol), get trow tcol t ])
    else []
  in
  let top_right =
    if top_middle != [] && middle_right != []
    then (
      let trow = row - 1 in
      let tcol = col + 1 in
      [ (trow, tcol), get trow tcol t ])
    else []
  in
  let bottom_left =
    if bottom_middle != [] && middle_left != []
    then (
      let trow = row + 1 in
      let tcol = col - 1 in
      [ (trow, tcol), get trow tcol t ])
    else []
  in
  let bottom_right =
    if bottom_middle != [] && middle_right != []
    then (
      let trow = row + 1 in
      let tcol = col + 1 in
      [ (trow, tcol), get trow tcol t ])
    else []
  in
  let neighbors =
    top_middle
    @ bottom_middle
    @ middle_left
    @ middle_right
    @ top_left
    @ top_right
    @ bottom_left
    @ bottom_right
  in
  neighbors
;;

(* (* Underpopulation Rule *)
   (*    Any live cell with fewer than two live neighbors dies *) *)
(* let rule_1 t = assert false *)
(* (* Survival Rule *)
   (*    Any live cell with two or three live neighbors lives on *) *)
(* let rule_2 t = assert false *)
(* (* Overpopulation Rule *)
   (*    Any live cell with more than three live neighbors dies *) *)
(* let rule_3 t = assert false *)
(* (* Reproduction Rule *)
   (*    Any dead cell with exactly three live neighbors becomes a live cell *) *)
(* let rule_4 t = assert false *)
(* let draw t = List.iter Tile.draw t.tiles *)