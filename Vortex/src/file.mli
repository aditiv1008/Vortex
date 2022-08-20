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

val empty_content : content
(** [empty_content] is an empty version of an open file's content. *)

val empty_file : open_file
(** [empty_file] is an empty version of an open file. *)

val empty_tools : tools
(** [empty_tools] is an empty version of an open file's tools. *)

val default_tools : tools
(** [default_tools] is a default version of an open file's tools, using
    the Merriam-Webster dictionary. *)

val split_string : string -> int -> string * string
(** [split_string str pos] returns a tuple [(s1, s2)] of substrings
    formed by splitting [str] at position [pos], such that [s1] has
    length [pos]. Raises: [InvalidIndex] if [pos] < 0 or [pos] > the
    length of [str]. *)

val delete_backwards_string : string -> int -> int -> string
(** [delete_backwards_string str pos size] returns a new string with
    [size] characters deleted going backwards from index [pos]. Raises:
    [InvalidIndex] if [pos] <= 0 or [pos] > length of [str],
    [InvalidSize] if [size] < 0 or [size] >= length of [str]. *)

val add_string : string -> string -> int -> string
(** [add_string orig_str added_str pos] returns a new string with
    [added_str] inserted into [orig_str] at position [pos]. Raises:
    [InvalidIndex] if [pos] < 0 or [pos] > the length of [str]. *)

val replace_string : string -> string -> string -> string
(** [replace_string orig final str] returns a new string with all
    instances of [orig] in [str] replaced with [final]. *)

val edit_content : (string -> string) -> int -> content -> content
(** [edit_content f ind cont] returns the content with line at index
    [ind] edited according to function [f], such that the new line would
    be [f] applied to said line. Raises: [InvalidIndex] if [ind] < 9 or
    [ind] >= the line length of [content]. *)

val add_line : string -> int -> open_file -> open_file
(** [add_line str ind o_file] adds string [str] to [o_file]'s text at
    index [ind]. Raises: [InvalidIndex] if [ind] < 0 or [ind] >= the
    line length of [o_file]. *)

val delete_line : int -> open_file -> open_file
(** [delete_line ind o_file] deletes the line at index [ind]. Raises:
    [InvalidIndex] if [ind] < 0 or [ind] >= the line length of [o_file]. *)

val combine_lines : int -> int -> open_file -> open_file
(** [combine_lines ind_dest ind_src o_file] moves the text from
    [ind_src] to the end of [ind_dest], deleting line at index [ind_src]
    along the way. Raises: [InvalidIndex] if [ind_dest] < 0 or
    [ind_dest] > the line length of [o_file] or [ind_src] < 0 or
    [ind_src] > the line length of [o_file]. *)

val split_line : int -> int -> open_file -> open_file
(** [split_line x_pos y_pos o_file] splits the line at index [y_pos]
    into two separate substrings, divided by index [x_pos]. They will be
    added into [o_file]'s text such that the first substring replaces
    the original line, and the second substring is inserted right after
    it. Raises: [InvalidIndex] if [x_pos] or [y_pos] < 0, if [x_pos] >
    the length of the line at [y_pos], or if [y_pos] > the length of
    [o_file] in lines. *)

val append_line : string -> open_file -> open_file
(** [append_line str o_file] appends line [str] to the end of [o_file]'s
    text. *)

val append_to_line : string -> int -> open_file -> open_file
(** [append_to_line str ind o_file] appends [str] to the end of the line
    at index [ind] in [o_file]. Raises: [InvalidIndex] if [ind] < 0 or
    [ind] >= the line length of [o_file]. *)

val add_to_line : string -> int -> int -> open_file -> open_file
(** [change_line change x_pos y_pos o_file] returns a new version of
    [o_file] with the line at index [pos_y] of string list [text]
    changed by inserting [change] at [pos_x] of said line. *)

val set_cursor_pos : int -> int -> open_file -> open_file
(** [set_cursor_pos x y o_file] changes the cursor position of [o_file]
    to match with [(x, y)]. *)

val change_cursor_pos : int -> int -> open_file -> open_file
(** [change_cursor_pos dx dy o_file] alters the cursor position by [dx]
    steps in the x-direction then by [dy] steps in the y-direction,
    keeping track of line changes along the way. If the cursor were to
    move past the start of the text or the end of the text by x-position
    only, it would instead return to the start of the text or end of the
    text respectively. If it would move out of bounds in the
    y-direction, then it maintains its original position. If it would
    move from a longer line to a shorter line, it would maintain the
    maintain the maximum between [dx] and the shorter line's length. *)

val delete_char_from_line : int -> int -> open_file -> open_file
(** [delete_char_from_line x_pos y_pos o_file] removes the character
    right before the cursor as if the cursor was at position
    [(x_pos, y_pos)]. If at the beginning of a line, it removes the
    newline seperator. Raises: [InvalidIndex] if [y_pos] < 0, [y_pos] >=
    the line length of [o_file], [x_pos] < 0, or [x_pos] > the length of
    the line at index [y_pos]. *)

val add_char_at_cursor : char -> open_file -> open_file
(** [add_char_at_cursor ch o_file] adds a character [ch] to [o_file] at
    the current cursor position, then adjusts said position to match. *)

val get_word_at_pos : int -> int -> open_file -> string
(** [get_word_at_pos x_pos y_pos o_file] gets the word right before the
    indicated position in [o_file] at [x_pos], [y_pos]. Returns an empty
    string if the character at said position is not ' ' or empty. *)

val get_word_at_cursor : open_file -> string
(** [get_word_at_cursor o_file] gets the word right before the cursor in
    [o_file]. Returns an empty string if the cursor is at a character
    that is not ' ' or empty. *)

val replace_string_file : string -> string -> open_file -> open_file
(** [replace_string_file orig final o_file] replaces every instance of
    the word [orig] in [o_file] with [final]. *)

val set_autocomplete_state : bool -> open_file -> open_file
(** [set_autocomplete_state b o_file] sets whether or not autocomplete
    functionality in [o_file] should be operating or not, depending on
    the value of [b]. *)

val get_autocomplete : open_file -> string
(** [get_autocomplete o_file] gets the current autocomplete suggestion
    in [o_file] at the cursor. *)

val change_open_file : (string * bool) list -> open_file -> open_file
(** [change_content changes cont] returns a new file content through a
    list of edits in [changes] applied to [cont]. *)

val load_from_file : string -> text_file
(** [load_from_file path] returns a new [text_file] from the given
    filepath [path]. If said [text_file] is empty, returns a file with a
    single empty line of text. Requires: [path] is a valid filepath to a
    text file. *)

val raw_text : open_file -> string
(** [raw_text o_file] returns the raw text of an open file [o_file],
    with lines seperated by newline characters. Raises: [InvalidFile] if
    [open_file] contains 0 total lines, regardless of whether each
    individual line is empty or not. Note that opening an empty file
    will still fulfill this condition (see: [load_from_file]). *)

val save_to_file : string -> open_file -> unit
(** [save_to_file path o_file] saves the raw text of open file [o_file]
    to the designated filepath [path], creating a new text file if there
    isn't already one there. Raises: [InvalidFile] if [open_file]
    contains 0 total lines, regardless of whether each individual line
    is empty or not. Note that opening an empty file will still fulfill
    this condition (see: [load_from_file]). *)

val save : text_file -> unit
(** [save text_file] saves the raw text of [text_file] to its designated
    filepath from when it was loaded. *)
