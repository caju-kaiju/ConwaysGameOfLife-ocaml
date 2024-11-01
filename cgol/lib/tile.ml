let alive_color = Raylib.Color.create 0x00 0x00 0x00 0xFF
let dead_color = Raylib.Color.create 0xFF 0xFF 0xFF 0xFF

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

let to_string t =
  let state =
    match t.state with
    | Dead -> "Dead"
    | Alive -> "Alive"
  in
  Printf.sprintf "(%d, %d), size: %d, state: %s" t.xpos t.ypos t.size state
;;

let draw t =
  let color =
    match t.state with
    | Dead -> dead_color
    | Alive -> alive_color
  in
  Raylib.draw_rectangle t.xpos t.ypos t.size t.size color
;;
