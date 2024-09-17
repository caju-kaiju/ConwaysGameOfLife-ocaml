[@@@ocaml.warning "-69-27-32-37-34-50"]

let color_black = Raylib.Color.create 255 255 255 255
let color_white = Raylib.Color.create 0 0 0 255

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
    ; color : Raylib.Color.t
    ; state : state
    }

  let dead_color = color_black
  let alive_color = color_white

  let create xpos ypos size state =
    let color =
      match state with
      | Dead -> dead_color
      | Alive -> alive_color
    in
    { xpos; ypos; size; color; state }
  ;;

  let draw t = Raylib.draw_rectangle t.xpos t.ypos t.size t.size t.color
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
  let size = 5 in
  let nrows = Window.height / size in
  let ncols = Window.width / size in
  let grid = Grid.create nrows ncols size in
  Window.setup ();
  loop grid
;;
