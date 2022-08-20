open Vortex

type event = OpenMenu of string

type input_changes = {
  events : event list;
  text_change : (string * bool) list;
}

(** [render_lines lines x_pos y_pos] prints out each string given by the
    string list [lines]. If autocomplete is on, it will also print the
    prediction [pred] at the location with x-pos [x_pos] and y-pos
    [y_pos]. *)
let rec render_lines lines x_pos y_pos pred : unit =
  let open ANSITerminal in
  let def_style = [ Foreground White; Background Black ] in
  match lines with
  | [] -> ()
  | h :: t ->
      if y_pos = 0 && pred <> "" then begin
        let s1, s2 = File.split_string h x_pos in
        print_string def_style s1;
        print_string [ Foreground White; Background Blue ] pred;
        print_string def_style s2
      end
      else begin
        print_string def_style (h ^ "\n");
        render_lines t x_pos (y_pos - 1) pred
      end

(** [render_screen current] clears the current screen then renders the
    new one according to the data in file [current]. *)
let render_screen (current : File.open_file) : unit =
  let x_pos, y_pos = current.text_cursor_pos in
  ANSITerminal.(
    erase Screen;
    set_cursor 1 1;
    render_lines current.content.text x_pos y_pos
      (File.get_autocomplete current);
    set_cursor (x_pos + 1) (y_pos + 1))

let rec caps_enabled_helper mods : bool * bool =
  match mods with
  | [] -> (false, false)
  | h :: t -> begin
      let caps, shift = caps_enabled_helper t in
      match h with
      | Sdlkeymod.CAPS -> (true, shift)
      | Sdlkeymod.LShift -> (caps, true)
      | Sdlkeymod.RShift -> (caps, true)
      | _ -> (caps, shift)
    end

(** [caps_enabled mods] returns whether or not caps are currently
    enabled on this keyboard given a list of keymods [mods]. *)
let caps_enabled mods : bool =
  let caps, shift = caps_enabled_helper mods in
  caps <> shift

let handle_keyboard_translation key caps : string =
  let open Sdlkeycode in
  let open Tools in
  match key with
  | Num0 -> choose caps ")" "0"
  | Num1 -> choose caps "!" "1"
  | Num2 -> choose caps "@" "2"
  | Num3 -> choose caps "#" "3"
  | Num4 -> choose caps "$" "4"
  | Num5 -> choose caps "%" "5"
  | Num6 -> choose caps "^" "6"
  | Num7 -> choose caps "&" "7"
  | Num8 -> choose caps "*" "8"
  | Num9 -> choose caps "(" "9"
  | Minus -> choose caps "_" "-"
  | Equals -> choose caps "+" "="
  | LeftBracket -> choose caps "{" "["
  | RightBracket -> choose caps "}" "]"
  | BackSlash -> choose caps "|" "\\"
  | SemiColon -> choose caps ":" ";"
  | Quote -> choose caps "\"" "'"
  | Comma -> choose caps "<" ","
  | Period -> choose caps ">" "."
  | Slash -> choose caps "?" "/"
  | BackQuote -> choose caps "~" "`"
  | Space -> " "
  | _ ->
      let str = to_string key in
      if String.length str > 1 || caps then str
      else String.lowercase_ascii str

(** [handle_keyboard key_event] returns the corresponding string of the
    [key_event]'s keycode. *)
let handle_keyboard (key_event : Sdlevent.keyboard_event) :
    string * bool =
  let caps = caps_enabled key_event.keymod in
  ( handle_keyboard_translation key_event.keycode caps,
    key_event.ke_state = Sdlevent.Pressed )

(** [render_input] takes in all input events in queue and sorts them out
    into a record with type [input_changes]. *)
let rec render_input () : input_changes =
  let open Sdlevent in
  match poll_event () with
  | None -> { events = []; text_change = [] }
  | Some event -> (
      let changes = render_input () in
      match event with
      | KeyDown event' ->
          {
            events = changes.events;
            text_change = handle_keyboard event' :: changes.text_change;
          }
      | _ -> changes)

(** [render_loop current] renders a single step of the text editor given
    file [current]. *)
let render_loop current =
  render_screen current;
  Vortex.File.change_open_file (render_input ()).text_change current

let main () =
  Sdl.(init_subsystem [ Init.(`VIDEO) ]);
  Sdlwindow.show
    (Sdlwindow.create ~title:"Vortex" ~pos:(`centered, `centered)
       ~dims:(400, 225) ~flags:[ Resizable ]);
  let current = ref File.empty_file in
  let running = ref true in
  while !running do
    current := render_loop !current;
    Unix.sleepf 0.01
  done

let () = main ()