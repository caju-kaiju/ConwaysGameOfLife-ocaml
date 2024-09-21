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
  type state =
    | Dead
    | Alive

  type t =
    { xpos : int
    ; ypos : int
    ; size : int
    ; state : state
    }

  let dead_color = color_white
  let alive_color = color_black
  let create xpos ypos size state = { xpos; ypos; size; state }
  let set_state new_state t = { xpos = t.xpos; ypos = t.ypos; size = t.size; state = new_state }

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

  let draw t = List.iter Tile.draw t.tiles

  let create nrows ncols tile_size =
    let tiles =
      List.init nrows (fun r ->
        List.init ncols (fun c -> Tile.create (c * tile_size) (r * tile_size) tile_size Tile.Dead))
      |> List.flatten
    in
    { nrows; ncols; tiles }
  ;;

  let set_tile_state row col state t =
    let rec aux tiles index acc =
      match tiles with
      | [] -> List.rev acc
      | h :: t ->
        if index = 0
        then (
          let new_tile = Tile.set_state state h in
          aux t (index - 1) (new_tile :: acc))
        else aux t (index - 1) (h :: acc)
    in
    let calc_index = (row * t.nrows) + col in
    let tiles = aux t.tiles calc_index [] in
    { nrows = t.nrows; ncols = t.ncols; tiles }
  ;;
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
    |> Grid.set_tile_state 0 0 Tile.Alive
    |> Grid.set_tile_state (nrows - 1) 0 Tile.Alive
    |> Grid.set_tile_state 0 (ncols - 1) Tile.Alive
    |> Grid.set_tile_state (nrows - 1) (ncols - 1) Tile.Alive
  in
  Window.setup ();
  loop grid
;;
