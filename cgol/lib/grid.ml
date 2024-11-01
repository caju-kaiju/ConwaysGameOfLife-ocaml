(*
   Represent a list as a grid. The length of the list must be equal to number of rows * number of columns.
*)

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

let create nrows ncols element_list =
  assert (List.length element_list = nrows * ncols);
  { nrows; ncols; elements = element_list }
;;

let new_element (grid_element : 'a element) (element : 'a) =
  { row = grid_element.row; col = grid_element.col; element }
;;

let calc_index t row col =
  assert (row >= 0);
  assert (col >= 0);
  assert (row < t.nrows);
  assert (col < t.ncols);
  (row * t.ncols) + col
;;

let calc_row t index = index / t.ncols
let calc_col t index row = index - (row * t.ncols)

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

let set_many (l : (int * int * 'a) list) (t : 'a t) =
  let rec aux l' acc =
    match l' with
    | [] -> acc
    | hd :: tl ->
      (match hd with
       | row, col, element -> aux tl (set row col element acc))
  in
  aux l t
;;

let iter f t = List.iter f t.elements

let find_all f t =
  let rec aux elements count acc =
    match elements with
    | [] -> acc
    | hd :: tl ->
      if f hd
      then (
        let row = calc_row t count in
        let col = calc_col t count row in
        let e = { row; col; element = hd } in
        aux tl (count + 1) (e :: acc))
      else aux tl (count + 1) acc
  in
  aux t.elements 0 []
;;

let get_neighbors row col t : 'a element list =
  let top_middle =
    if row > 0
    then (
      let trow = row - 1 in
      let tcol = col in
      let e = get trow tcol t in
      [ { row = trow; col = tcol; element = e } ])
    else []
  in
  let bottom_middle =
    if row < t.nrows - 1
    then (
      let trow = row + 1 in
      let tcol = col in
      let e = get trow tcol t in
      [ { row = trow; col = tcol; element = e } ])
    else []
  in
  let middle_left =
    if col > 0
    then (
      let trow = row in
      let tcol = col - 1 in
      let e = get trow tcol t in
      [ { row = trow; col = tcol; element = e } ])
    else []
  in
  let middle_right =
    if col < t.ncols - 1
    then (
      let trow = row in
      let tcol = col + 1 in
      let e = get trow tcol t in
      [ { row = trow; col = tcol; element = e } ])
    else []
  in
  let top_left =
    if top_middle != [] && middle_left != []
    then (
      let trow = row - 1 in
      let tcol = col - 1 in
      let e = get trow tcol t in
      [ { row = trow; col = tcol; element = e } ])
    else []
  in
  let top_right =
    if top_middle != [] && middle_right != []
    then (
      let trow = row - 1 in
      let tcol = col + 1 in
      let e = get trow tcol t in
      [ { row = trow; col = tcol; element = e } ])
    else []
  in
  let bottom_left =
    if bottom_middle != [] && middle_left != []
    then (
      let trow = row + 1 in
      let tcol = col - 1 in
      let e = get trow tcol t in
      [ { row = trow; col = tcol; element = e } ])
    else []
  in
  let bottom_right =
    if bottom_middle != [] && middle_right != []
    then (
      let trow = row + 1 in
      let tcol = col + 1 in
      let e = get trow tcol t in
      [ { row = trow; col = tcol; element = e } ])
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
