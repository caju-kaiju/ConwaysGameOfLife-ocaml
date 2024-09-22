[@@@ocaml.warning "-69-27-32-37-34-50"]

let color_black = Raylib.Color.create 0 0 0 255
let color_white = Raylib.Color.create 255 255 255 255

module Window = struct
  let width = 800
  let height = 1200
  let bg_color = color_black

  let setup () =
    Raylib.init_window width height "Conway's Game Of Life";
    Raylib.set_target_fps 60;
    ()
  ;;
end

module Tile = struct
  let dead_color = color_white
  let alive_color = color_black

  type state =
    | Dead
    | Alive

  type t =
    { xpos : int
    ; ypos : int
    ; size : int
    ; state : state
    }

  let create xpos ypos size state = { xpos; ypos; size; state }
  let new_state state t = { xpos = t.xpos; ypos = t.ypos; size = t.size; state }

  let draw t =
    let color =
      match t.state with
      | Dead -> dead_color
      | Alive -> alive_color
    in
    Raylib.draw_rectangle t.xpos t.ypos t.size t.size color
  ;;
end

module Grid = struct
  type t =
    { nrows : int
    ; ncols : int
    ; tiles : Tile.t list
    }

  let create nrows ncols tile_size =
    let tiles =
      List.init nrows (fun r ->
        List.init ncols (fun c -> Tile.create (c * tile_size) (r * tile_size) tile_size Tile.Dead))
      |> List.flatten
    in
    { nrows; ncols; tiles }
  ;;

  let calc_index row col t = (row * t.ncols) + col

  let update_tile_state row col state t =
    let rec aux tiles index acc =
      match tiles with
      | [] -> List.rev acc
      | h :: t ->
        if index = 0
        then (
          let new_tile = h |> Tile.new_state state in
          aux t (index - 1) (new_tile :: acc))
        else aux t (index - 1) (h :: acc)
    in
    let index = calc_index row col t in
    let tiles = aux t.tiles index [] in
    { nrows = t.nrows; ncols = t.ncols; tiles }
  ;;

  let find_neighbors row col t =
    let target_index = calc_index row col t in
    let top_left = target_index - t.ncols - 1 in
    let top_middle = target_index - t.ncols in
    let top_right = target_index - t.ncols + 1 in
    let middle_left = target_index - 1 in
    let middle_right = target_index + 1 in
    let bottom_left = target_index + t.ncols - 1 in
    let bottom_middle = target_index + t.ncols in
    let bottom_right = target_index + t.ncols + 1 in
    [ top_left; top_middle; top_right; middle_left; middle_right; bottom_left; bottom_middle; bottom_right ]
    |> List.filter (fun x -> x > 0)
    |> List.filter (fun x -> x < t.nrows * t.ncols)
  ;;

  (* Underpopulation Rule
     Any live cell with fewer than two live neighbours dies *)
  let rule_1 t = assert false

  (* Survival Rule
     Any live cell with two or three live neighbours lives on *)
  let rule_2 t = assert false

  (* Overpopulation Rule
     Any live cell with more than three live neighbours dies *)
  let rule_3 t = assert false

  (* Reproduction Rule
     Any dead cell with exactly three live neighbours becomes a live cell *)
  let rule_4 t = assert false
  let draw t = List.iter Tile.draw t.tiles
end

let rec loop grid =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
    let new_grid = grid in
    Raylib.begin_drawing ();
    Raylib.clear_background Window.bg_color;
    Grid.draw new_grid;
    Raylib.end_drawing ();
    loop new_grid
;;

let () =
  let size = 10 in
  let nrows = Window.height / size in
  let ncols = Window.width / size in
  let grid =
    Grid.create nrows ncols size
    |> Grid.update_tile_state 0 0 Tile.Alive
    |> Grid.update_tile_state (nrows - 1) 0 Tile.Alive
    |> Grid.update_tile_state 0 (ncols - 1) Tile.Alive
    |> Grid.update_tile_state (nrows - 1) (ncols - 1) Tile.Alive
  in
  Window.setup ();
  loop grid
;;
