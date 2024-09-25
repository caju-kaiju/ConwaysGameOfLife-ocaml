[@@@ocaml.warning "-69-27-32-37-34-50"]

let color_black = Raylib.Color.create 0 0 0 255
let color_white = Raylib.Color.create 255 255 255 255

module Window = struct
  let width = 1200
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

  (*
     I greatly dislike this. This needs a better solution. This is the best I have while doing two things:
     1. Avoid associating a tile directly with its neighbors. I would like it to be a calculation.
     2. Using references to solve this issue.

     The main issue is finding a way to conditionally add to a list, but take no action otherwise. This could
     probably be solved with a reference, but I am choosing to avoid using references right now.

     TODO: Double check these index calculations
  *)
  let find_neighbors row col t =
    let calc_top = if row != 0 then true else false in
    let calc_bottom = if row != t.nrows - 1 then true else false in
    let calc_left = if col != 0 then true else false in
    let calc_right = if col != t.ncols - 1 then true else false in
    let target_index = calc_index row col t in
    let top_middle = if calc_top then [ target_index - t.ncols ] else [] in
    let bottom_middle = if calc_bottom then [ target_index + t.ncols ] else [] in
    let middle_left = if calc_left then [ target_index - 1 ] else [] in
    let middle_right = if calc_right then [ target_index + 1 ] else [] in
    let top_left = if calc_top && calc_left then [ target_index - t.ncols - 1 ] else [] in
    let top_right = if calc_top && calc_right then [ target_index - t.ncols + 1 ] else [] in
    let bottom_left = if calc_bottom && calc_left then [ target_index + t.ncols - 1 ] else [] in
    let bottom_right = if calc_bottom && calc_right then [ target_index + t.ncols + 1 ] else [] in
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

  (* Underpopulation Rule
     Any live cell with fewer than two live neighbors dies *)
  let rule_1 t = assert false

  (* Survival Rule
     Any live cell with two or three live neighbors lives on *)
  let rule_2 t = assert false

  (* Overpopulation Rule
     Any live cell with more than three live neighbors dies *)
  let rule_3 t = assert false

  (* Reproduction Rule
     Any dead cell with exactly three live neighbors becomes a live cell *)
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
