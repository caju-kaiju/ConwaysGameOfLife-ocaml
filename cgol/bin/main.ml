[@@@ocaml.warning "-69-27-32-37-34-50"]

open Lib

let color_orange = Raylib.Color.create 0xFF 0xA5 0x00 0xFF

module Utility = struct
  let highlight_neighbors row col grid =
    let rec aux (neighbors' : 'a Grid.element list) grid' =
      match neighbors' with
      | [] -> grid'
      | h :: t ->
        let trow = h.row in
        let tcol = h.col in
        let tile = h.element |> Tile.new_state Tile.Test in
        aux t (Grid.set trow tcol tile grid')
    in
    aux (Grid.get_neighbors row col grid) grid
  ;;

  let log_info str = "\n" ^ str |> Raylib.trace_log (Raylib.TraceLogLevel.to_int Raylib.TraceLogLevel.Info)
  let log_tile (tile : Tile.t) = log_info (Tile.to_string tile)

  let log_grid_element (e : 'a Grid.element) =
    let info = Printf.sprintf "(%d, %d): %s" e.row e.col (Tile.to_string e.element) in
    log_info info
  ;;
end

let random_population (n : int) (grid : Tile.t Grid.t) =
  let rec aux n' grid =
    match n' <= 0 with
    | true -> grid
    | false ->
      let row = Raylib.get_random_value 0 (grid.Grid.nrows - 1) in
      let col = Raylib.get_random_value 0 (grid.Grid.ncols - 1) in
      let tile = Grid.get row col grid in
      let new_grid = grid |> Grid.set row col (Tile.new_state Tile.Alive tile) in
      aux (n' - 1) new_grid
  in
  aux n grid
;;

module Window = struct
  let width = 1200
  let height = 1200
  let bg_color = color_orange

  let setup () =
    Raylib.init_window width height "Conway's Game Of Life";
    Raylib.set_window_position 100 100;
    Raylib.set_target_fps 60;
    ()
  ;;
end

(* Underpopulation Rule
   Any live cell with fewer than two live neighbors dies *)
(* (* Survival Rule *)
   (*    Any live cell with two or three live neighbors lives on *) *)
(* (* Overpopulation Rule *)
   (*    Any live cell with more than three live neighbors dies *) *)
(* (* Reproduction Rule *)
   (*    Any dead cell with exactly three live neighbors becomes a live cell *) *)

(* Underpopulation Rule
   Any live cell with fewer than two live neighbors dies

   1. Get all the alive tiles.
   2. For each live tile, get its neighbors.
   3. Filter out neighbors that are not Alive.
   4. If there are less than 2 neighbors, die.
   returns a list of Tile.t which have died.
*)
let underpopulation_rule (grid : Tile.t Grid.t) : Tile.t Grid.element list =
  let alive_tiles = Grid.find_all (fun (tile : Tile.t) -> tile.state = Tile.Alive) grid in
  let rec aux (alive_tiles' : Tile.t Grid.element list) acc =
    match alive_tiles' with
    | [] -> acc
    | hd :: tl ->
      let alive_neighbors =
        Grid.get_neighbors hd.row hd.col grid
        |> List.filter (fun (e : Tile.t Grid.element) -> e.element.state = Tile.Alive)
      in
      let death = List.length alive_neighbors < 2 in
      if death
      then (
        let dead_tile = Tile.new_state Tile.Dead hd.element in
        let dead_element = Grid.new_element hd dead_tile in
        aux tl (dead_element :: acc))
      else aux tl acc
  in
  aux alive_tiles []
;;

let rec loop (grid : Tile.t Grid.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
    let new_grid = grid in
    Raylib.begin_drawing ();
    Raylib.clear_background Window.bg_color;
    Grid.iter Tile.draw new_grid;
    underpopulation_rule grid |> List.length |> print_int;
    print_newline ();
    Raylib.end_drawing ();
    loop new_grid
;;

let () =
  Unix.gettimeofday () |> truncate |> Unsigned.UInt.of_int |> Raylib.set_random_seed;
  let size = 10 in
  let nrows = Window.height / size in
  let ncols = Window.width / size in
  let tiles =
    List.init nrows (fun r -> List.init ncols (fun c -> Tile.create (c * size) (r * size) size Tile.Dead))
    |> List.flatten
  in
  let grid = Grid.create nrows ncols tiles in
  (* let test_grid = *)
  (* random_population 500 grid *)
  let t1 = grid |> Grid.get 0 0 |> Tile.new_state Tile.Alive in
  let t2 = grid |> Grid.get 0 (grid.Grid.ncols - 1) |> Tile.new_state Tile.Alive in
  let t3 = grid |> Grid.get (grid.Grid.nrows - 1) 0 |> Tile.new_state Tile.Alive in
  let t4 = grid |> Grid.get (grid.Grid.nrows - 1) (grid.Grid.ncols - 1) |> Tile.new_state Tile.Alive in
  let t5 = grid |> Grid.get (grid.Grid.nrows / 2) (grid.Grid.nrows / 2) |> Tile.new_state Tile.Alive in
  let test_grid =
    grid
    |> Grid.set_many
         [ 0, 0, t1
         ; 0, grid.Grid.ncols - 1, t2
         ; grid.Grid.nrows - 1, 0, t3
         ; grid.Grid.nrows - 1, grid.Grid.ncols - 1, t4
         ; grid.Grid.nrows / 2, grid.Grid.nrows / 2, t5
         ]
  in
  Window.setup ();
  loop test_grid
;;
