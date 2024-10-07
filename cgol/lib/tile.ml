let alive_color = Raylib.Color.create 0x00 0x00 0x00 0xFF
let dead_color = Raylib.Color.create 0xFF 0xFF 0xFF 0xFF
let test_color = Raylib.Color.create 0xFF 0xA5 0x00 0xFF

type state =
  | Dead
  | Alive
  | Test

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
    | Test -> test_color
  in
  Raylib.draw_rectangle t.xpos t.ypos t.size t.size color
;;