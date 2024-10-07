[@@@ocaml.warning "-69-27-32-37-34-50"]

open Lib

let color_orange = Raylib.Color.create 0xFF 0xA5 0x00 0xFF

module Utility = struct
  let highlight_neighbors row col grid =
    let rec aux neighbors' grid' =
      match neighbors' with
      | [] -> grid'
      | h :: t ->
        let trow = h |> fst |> fst in
        let tcol = h |> fst |> snd in
        let tile = h |> snd |> Tile.new_state Tile.Test in
        aux t (Grid.set trow tcol tile grid')
    in
    aux (Grid.get_neighbors row col grid) grid
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
    Raylib.set_target_fps 60;
    ()
  ;;
end

let rec loop (grid : Tile.t Grid.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
    let new_grid = grid in
    Raylib.begin_drawing ();
    Raylib.clear_background Window.bg_color;
    Grid.iter Tile.draw new_grid;
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
  (* let grid = Grid.create nrows ncols tiles |> random_population 3 in *)
  let grid = Grid.create nrows ncols tiles in
  let t1 = grid |> Grid.get 0 0 |> Tile.new_state Tile.Alive in
  let t2 = grid |> Grid.get 0 (grid.Grid.ncols - 1) |> Tile.new_state Tile.Alive in
  let t3 = grid |> Grid.get (grid.Grid.nrows - 1) 0 |> Tile.new_state Tile.Alive in
  let t4 = grid |> Grid.get (grid.Grid.nrows - 1) (grid.Grid.ncols - 1) |> Tile.new_state Tile.Alive in
  let t5 = grid |> Grid.get (grid.Grid.nrows / 2) (grid.Grid.nrows / 2) |> Tile.new_state Tile.Alive in
  let test_grid =
    grid
    |> Grid.set 0 0 t1
    |> Grid.set 0 (grid.Grid.ncols - 1) t2
    |> Grid.set (grid.Grid.nrows - 1) 0 t3
    |> Grid.set (grid.Grid.nrows - 1) (grid.Grid.ncols - 1) t4
    |> Grid.set (grid.Grid.nrows / 2) (grid.Grid.nrows / 2) t5
    |> Utility.highlight_neighbors 0 0
    |> Utility.highlight_neighbors 0 (grid.Grid.ncols - 1)
    |> Utility.highlight_neighbors (grid.Grid.nrows - 1) 0
    |> Utility.highlight_neighbors (grid.Grid.nrows - 1) (grid.Grid.ncols - 1)
    |> Utility.highlight_neighbors (grid.Grid.nrows / 2) (grid.Grid.nrows / 2)
  in
  Window.setup ();
  loop test_grid
;;
