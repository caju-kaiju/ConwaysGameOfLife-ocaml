[@@@ocaml.warning "-69-27-32-37-34-50"]

open Lib

let color_orange = Raylib.Color.create 0xFF 0xA5 0x00 0xFF

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
  let width = 800
  let height = 800
  let bg_color = color_orange

  let setup () =
    Raylib.init_window width height "Conway's Game Of Life";
    Raylib.set_window_position 100 100;
    Raylib.set_target_fps 60;
    ()
  ;;
end

(*
   collect: How to find all appropriate tiles
   filter: Filter out non-relavent neighbors
   rule: Rule to apply based on filtered neighbors
   transform: How to handle the Tile if rule is met
*)
let apply_rule ~collect ~filter ~rule ~transform grid =
  let tiles = Grid.find_all collect grid in
  let rec aux (tiles' : Tile.t Grid.element list) acc =
    match tiles' with
    | [] -> acc
    | hd :: tl ->
      let neighbors = Grid.get_neighbors hd.row hd.col grid |> List.filter filter in
      if rule neighbors
      then (
        let new_tile = transform hd.element in
        let new_element = Grid.new_element hd new_tile in
        aux tl (new_element :: acc))
      else aux tl acc
  in
  aux tiles []
;;

(* Underpopulation Rule
   Any live cell with fewer than two live neighbors dies *)
let apply_rule_underpopulation =
  apply_rule
    ~collect:(fun (tile : Tile.t) -> tile.state = Tile.Alive)
    ~filter:(fun (e : Tile.t Grid.element) -> e.element.state = Tile.Alive)
    ~rule:(fun (neighbors : Tile.t Grid.element list) -> List.length neighbors < 2)
    ~transform:(fun (tile : Tile.t) -> Tile.new_state Tile.Dead tile)
;;

(* Overpopulation Rule
   Any live cell with more than three live neighbors dies *)
let apply_rule_overpopulation =
  apply_rule
    ~collect:(fun (tile : Tile.t) -> tile.state = Tile.Alive)
    ~filter:(fun (e : Tile.t Grid.element) -> e.element.state = Tile.Alive)
    ~rule:(fun (neighbors : Tile.t Grid.element list) -> List.length neighbors > 3)
    ~transform:(fun (tile : Tile.t) -> Tile.new_state Tile.Dead tile)
;;

(* Reproduction Rule
   Any dead cell with exactly three live neighbors becomes a live cell *)
let appy_rule_reproduction =
  apply_rule
    ~collect:(fun (tile : Tile.t) -> tile.state = Tile.Dead)
    ~filter:(fun (e : Tile.t Grid.element) -> e.element.state = Tile.Alive)
    ~rule:(fun (neighbors : Tile.t Grid.element list) -> List.length neighbors = 3)
    ~transform:(fun (tile : Tile.t) -> Tile.new_state Tile.Alive tile)
;;

let parrallel_rule_apply pool grid =
  let underpopulation_a = Domainslib.Task.async pool (fun _ -> apply_rule_underpopulation grid) in
  let overopulation_a = Domainslib.Task.async pool (fun _ -> apply_rule_overpopulation grid) in
  let repdroduction_a = Domainslib.Task.async pool (fun _ -> appy_rule_reproduction grid) in
  let generation_change =
    Domainslib.Task.await pool underpopulation_a
    @ Domainslib.Task.await pool overopulation_a
    @ Domainslib.Task.await pool repdroduction_a
    |> List.map (fun (e : Tile.t Grid.element) -> e.row, e.col, e.element)
  in
  Grid.set_many generation_change grid
;;

let inital_draw grid =
  Raylib.begin_drawing ();
  Raylib.clear_background Window.bg_color;
  Grid.iter Tile.draw grid;
  Raylib.end_drawing ()
;;

let rec wait_for_go () =
  Raylib.poll_input_events ();
  if Raylib.is_key_pressed Raylib.Key.Space then () else wait_for_go ()
;;

let rec loop (grid : Tile.t Grid.t) pool =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
    let new_grid = Domainslib.Task.run pool (fun _ -> parrallel_rule_apply pool grid) in
    Raylib.begin_drawing ();
    Raylib.clear_background Window.bg_color;
    Grid.iter Tile.draw new_grid;
    Raylib.end_drawing ();
    loop new_grid pool
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
  let grid = Grid.create nrows ncols tiles |> random_population 1000 in
  let pool = Domainslib.Task.setup_pool ~num_domains:3 () in
  Window.setup ();
  inital_draw grid;
  wait_for_go ();
  loop grid pool;
  Domainslib.Task.teardown_pool pool
;;
