type content = {
  lines_len : int;
  text : string list;
}

type tools = {
  trie : Predict.t;
  autocomplete : bool;
}

type open_file = {
  content : content;
  text_cursor_pos : int * int;
  tools : tools;
}

type text_file = {
  open_file : open_file;
  name : string;
  file_path : string;
}

exception InvalidIndex
exception InvalidFile
exception InvalidSize

let init_trie file_path =
  Yojson.Basic.from_file file_path
  |> Yojson.Basic.Util.to_assoc |> Tools.tail_rec_map fst
  |> Tools.make_dict

let empty_content = { lines_len = 1; text = [ "" ] }
let empty_tools = { trie = Predict.empty; autocomplete = false }

let default_tools =
  { trie = init_trie "src/words.json"; autocomplete = true }

let empty_file =
  {
    content = empty_content;
    text_cursor_pos = (0, 0);
    tools = default_tools;
  }

let split_string str pos : string * string =
  let length = String.length str in
  if pos = 0 then ("", str)
  else if pos = length then (str, "")
  else if pos < 0 || pos > length then raise InvalidIndex
  else (String.sub str 0 pos, String.sub str pos (length - pos))

let delete_backwards_string str pos size : string =
  let length = String.length str in
  if pos <= 0 || pos > length then raise InvalidIndex
  else if size < 0 || size > length then raise InvalidSize
  else if pos < size then String.sub str (pos + 1) (length - pos)
  else if pos = length - 1 then String.sub str 0 (length - size)
  else
    begin
      String.sub str 0 (pos - size)
      ^ String.sub str pos (length - pos)
    end

let add_string orig_str added_str pos : string =
  let s1, s2 = split_string orig_str pos in
  s1 ^ added_str ^ s2

let replace_string orig final str : string =
  let o_len = String.length orig in
  let s_len = String.length str in
  let buf = Buffer.create s_len in
  let i = ref 0 in
  while !i < s_len do
    if s_len - !i >= o_len then
      if String.sub str !i o_len = orig then (
        Buffer.add_string buf final;
        i := !i + o_len)
      else (
        Buffer.add_char buf str.[!i];
        i := !i + 1)
    else (
      Buffer.add_string buf (String.sub str !i (s_len - !i));
      i := s_len)
  done;
  Buffer.contents buf

let set_cursor_pos x y o_file : open_file =
  let x = min x (List.nth o_file.content.text y |> String.length) in
  {
    content = o_file.content;
    text_cursor_pos = (x, y);
    tools = o_file.tools;
  }

let rec change_cursor_pos_helper dx dy carry_dy o_file : open_file =
  let lines_len = o_file.content.lines_len in
  let text = o_file.content.text in
  let x, y = o_file.text_cursor_pos in
  let nx, ny = (x + dx, y + dy + carry_dy) in
  if ny < 0 || ny >= lines_len then o_file
  else if nx < 0 then
    if ny > 0 then
      change_cursor_pos_helper
        ((y + carry_dy - 1 |> List.nth text |> String.length) + dx + 1)
        dy (carry_dy - 1) o_file
    else set_cursor_pos 0 0 o_file
  else
    let y_line_len = List.nth text (y + carry_dy) |> String.length in
    if nx > y_line_len then
      if ny < lines_len - 1 then
        change_cursor_pos_helper
          (dx - 1 - y_line_len)
          dy (carry_dy + 1) o_file
      else
        set_cursor_pos
          (lines_len - 1 |> List.nth text |> String.length)
          (lines_len - 1) o_file
    else set_cursor_pos nx ny o_file

let rec change_cursor_pos dx dy o_file : open_file =
  change_cursor_pos_helper dx dy 0 o_file

let rec edit_content_helper f ind text : string list =
  match text with
  | [] -> raise InvalidIndex
  | h :: t ->
      if ind = 0 then f h :: t
      else
        h
        :: edit_content_helper f
             begin
               ind - 1
             end t

let edit_content f ind cont : content =
  if ind < 0 || ind >= cont.lines_len then raise InvalidIndex
  else
    {
      text = edit_content_helper f ind cont.text;
      lines_len = cont.lines_len;
    }

let add_line str ind o_file : open_file =
  let cont = o_file.content in
  {
    content =
      {
        text = Tools.insert str ind cont.text;
        lines_len = cont.lines_len + 1;
      };
    text_cursor_pos = o_file.text_cursor_pos;
    tools = o_file.tools;
  }

let delete_line ind o_file : open_file =
  let x_pos, y_pos = o_file.text_cursor_pos in
  let cont = o_file.content in
  {
    content =
      {
        text = Tools.remove ind cont.text;
        lines_len = cont.lines_len - 1;
      };
    text_cursor_pos = (x_pos, if y_pos > ind then y_pos - 1 else y_pos);
    tools = o_file.tools;
  }

let append_line str o_file : open_file =
  let cont = o_file.content in
  {
    content =
      { text = cont.text @ [ str ]; lines_len = cont.lines_len + 1 };
    text_cursor_pos = o_file.text_cursor_pos;
    tools = o_file.tools;
  }

let rec split_line_helper x_pos y_pos list : string list =
  match list with
  | [] -> raise InvalidIndex
  | h :: t ->
      if y_pos = 0 then
        let s1, s2 = split_string h x_pos in
        s1 :: s2 :: t
      else h :: split_line_helper x_pos (y_pos - 1) t

let split_line x_pos y_pos o_file : open_file =
  let cont = o_file.content in
  if y_pos < 0 || x_pos < 0 || y_pos >= cont.lines_len then
    raise InvalidIndex
  else
    {
      content =
        {
          text = split_line_helper x_pos y_pos cont.text;
          lines_len = cont.lines_len + 1;
        };
      text_cursor_pos = o_file.text_cursor_pos;
      tools = o_file.tools;
    }

let rec append_to_line str ind o_file : open_file =
  {
    content = edit_content (fun line -> line ^ str) ind o_file.content;
    text_cursor_pos = o_file.text_cursor_pos;
    tools = o_file.tools;
  }

let rec add_to_line str x_pos y_pos o_file : open_file =
  {
    content =
      edit_content
        begin
          fun line ->
          add_string line str x_pos
        end
        y_pos o_file.content;
    text_cursor_pos = o_file.text_cursor_pos;
    tools = o_file.tools;
  }

let combine_lines ind_dest ind_src o_file : open_file =
  let cont = o_file.content in
  if
    ind_dest < 0
    || ind_dest > cont.lines_len
    || ind_src < 0
    || ind_src > cont.lines_len
  then raise InvalidIndex
  else
    append_to_line (List.nth cont.text ind_src) ind_dest o_file
    |> delete_line ind_src

let rec delete_char_from_line_helper x_pos y_pos text : string list =
  match text with
  | [] -> raise InvalidIndex
  | h :: t ->
      if y_pos = 0 then delete_backwards_string h x_pos 1 :: t
      else
        h
        :: delete_char_from_line_helper x_pos
             begin
               y_pos - 1
             end t

let delete_char_from_line x_pos y_pos o_file : open_file =
  let x_pos_c, y_pos_c = o_file.text_cursor_pos in
  let cont = o_file.content in
  if x_pos = 0 then
    if y_pos = 0 then o_file
    else
      combine_lines (y_pos - 1) y_pos
        (if y_pos_c = y_pos then change_cursor_pos (-1) 0 o_file
        else o_file)
  else
    {
      content =
        {
          text = delete_char_from_line_helper x_pos y_pos cont.text;
          lines_len = cont.lines_len;
        };
      text_cursor_pos =
        ( (if y_pos = y_pos_c && x_pos_c >= x_pos then x_pos_c - 1
          else x_pos_c),
          y_pos_c );
      tools = o_file.tools;
    }

let replace_string_file orig final o_file : open_file =
  {
    content =
      {
        text =
          Tools.tail_rec_map
            (replace_string orig final)
            o_file.content.text;
        lines_len = o_file.content.lines_len;
      };
    tools = o_file.tools;
    text_cursor_pos = (0, 0);
  }

let set_autocomplete_state b o_file : open_file =
  let tools = o_file.tools in
  {
    content = o_file.content;
    text_cursor_pos = o_file.text_cursor_pos;
    tools = { trie = tools.trie; autocomplete = b };
  }

let add_char_at_cursor ch o_file : open_file =
  let x_pos, y_pos = o_file.text_cursor_pos in
  add_to_line (String.make 1 ch) x_pos y_pos o_file
  |> change_cursor_pos 1 0
  |> set_autocomplete_state true

let add_text_at_cursor s o_file : open_file =
  String.to_seq s
  |> Seq.fold_left
       begin
         fun o c ->
         add_char_at_cursor c o
       end
       o_file

let get_word_at_pos_fold curr_str e tar_i : string * bool * bool =
  let word, reached, success = curr_str in
  let ind, x = e in
  if ind = tar_i then (word, true, x = ' ')
  else
    let new_word =
      if reached then word
      else if x = ' ' then ""
      else
        begin
          word ^ Char.escaped x
        end
    in
    (new_word, reached, success)

let rec get_word_at_pos_helper x_pos y_pos text : string =
  match text with
  | [] -> raise InvalidIndex
  | h :: t ->
      if y_pos = 0 then
        let word, reached, success =
          String.to_seqi h
          |> Seq.fold_left
               begin
                 fun curr e ->
                 get_word_at_pos_fold curr e x_pos
               end
               ("", false, false)
        in
        if reached = success then word else ""
      else
        get_word_at_pos_helper x_pos
          begin
            y_pos - 1
          end t

let get_word_at_pos x_pos y_pos o_file : string =
  get_word_at_pos_helper x_pos y_pos o_file.content.text

let get_word_at_cursor o_file : string =
  let x_pos, y_pos = o_file.text_cursor_pos in
  get_word_at_pos x_pos y_pos o_file

let get_autocomplete o_file : string =
  let word = get_word_at_cursor o_file in
  let tools = o_file.tools in
  if word = "" || not tools.autocomplete then ""
  else Predict.find word tools.trie

let raw_text o_file : string =
  match o_file.content.text with
  | [] -> raise InvalidFile
  | h :: t ->
      begin
        h
        ^ List.fold_left
            begin
              fun str1 str2 ->
              str1 ^ "\n" ^ str2
            end
            "" t
      end

let save_to_file path o_file =
  let oc = open_out path in
  output_string oc (raw_text o_file);
  close_out oc

let rec save_to_file_helper s o_file =
  match read_line () with
  | "" -> save_to_file_helper s o_file
  | path -> begin
      match save_to_file path o_file with
      | exception Sys_error m ->
          print_endline "sorry wrong directory";
          save_to_file_helper s o_file
      | _ ->
          save_to_file path o_file;
          o_file
    end

let prompt_save o_file =
  print_newline ();
  print_endline "Type filename to save: ";
  save_to_file_helper () o_file

let prompt_replace o_file =
  print_newline ();
  print_endline "Replace: ";
  let orig = read_line () in
  print_endline "With: ";
  let final = read_line () in
  replace_string_file orig final o_file

let change_open_file_special change o_file : open_file =
  let x_pos, y_pos = o_file.text_cursor_pos in
  let h_str, h_state = change in
  match h_str with
  | "Escape" ->
      if get_autocomplete o_file = "" then begin
        Sdlquit.quit ();
        exit 0
      end
      else set_autocomplete_state false o_file
  | "Return" ->
      begin
        split_line x_pos y_pos o_file
        |> change_cursor_pos 1 0
      end
  | "Left" -> change_cursor_pos (-1) 0 o_file
  | "Right" -> change_cursor_pos 1 0 o_file
  | "Up" -> change_cursor_pos 0 (-1) o_file
  | "Down" -> change_cursor_pos 0 1 o_file
  | "Backspace" -> delete_char_from_line x_pos y_pos o_file
  | "LCtrl" -> prompt_save o_file
  | "RCtrl" -> prompt_save o_file
  | "Tab" ->
      if o_file.tools.autocomplete then
        add_text_at_cursor (get_autocomplete o_file) o_file
      else o_file
  | "LAlt" -> prompt_replace o_file
  | "RAlt" -> prompt_replace o_file
  | _ -> o_file

let rec change_open_file changes o_file : open_file =
  match changes with
  | [] -> o_file
  | (h_str, h_state) :: t ->
      if String.length h_str > 1 then
        change_open_file_special (h_str, h_state) o_file
      else add_char_at_cursor h_str.[0] o_file

let rec load_from_file_helper file : string list * int =
  try
    let h = input_line file in
    let t = load_from_file_helper file in
    (h :: fst t, snd t + 1)
  with End_of_file -> ([], 0)

let load_from_file path : text_file =
  let ic = open_in path in
  let file_data = load_from_file_helper ic in
  close_in ic;
  {
    open_file =
      {
        content =
          {
            text =
              (let text = fst file_data in
               if List.length text = 0 then [ "" ] else text);
            lines_len = snd file_data;
          };
        text_cursor_pos = (0, 0);
        tools =
          { trie = init_trie "src/words.json"; autocomplete = true };
      };
    name = Filename.basename path;
    file_path = path;
  }

let save text_file =
  let oc = open_out text_file.file_path in
  output_string oc (raw_text text_file.open_file);
  close_out oc
