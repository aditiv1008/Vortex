(** Test Plan: We manually test the Terminal GUI for Vortex as well as
    saving to files. We automatically test all other features of Vortex
    which include file handling, text manipulation, word prediction
    (autocomplete), and find and replace. OUnit is used to test the
    modules file.ml, predict.ml, and tools.ml. We used black-box
    testing. This testing system ensures that we are able to abstract
    away the modules for handling text and testing them for what they
    should output, and then manually testing the GUI by trying various
    strange key sequences, checking that the system still behaves as
    expected and doesn't crash. *)

open OUnit2
open Vortex

let make_test_predict (name : string) e1 e2 =
  name >:: fun _ -> assert_equal e1 e2 ~printer:String.escaped

let make_test_get_word (name : string) x_pos y_pos o_file exp =
  name >:: fun _ ->
  assert_equal exp
    (File.get_word_at_pos x_pos y_pos o_file)
    ~printer:String.escaped

let make_test_split_string (name : string) str pos exp =
  name >:: fun _ -> assert_equal exp (File.split_string str pos)

let make_test_delete_backwards_string (name : string) str pos size exp =
  name >:: fun _ ->
  assert_equal exp
    (File.delete_backwards_string str pos size)
    ~printer:String.escaped

let make_test_add_string (name : string) str add pos exp =
  name >:: fun _ ->
  assert_equal exp (File.add_string str add pos) ~printer:String.escaped

let make_test_replace_string (name : string) orig final str exp =
  name >:: fun _ ->
  assert_equal exp
    (File.replace_string orig final str)
    ~printer:String.escaped

let make_test_raw_text (name : string) o_file exp =
  name >:: fun _ ->
  assert_equal exp (File.raw_text o_file) ~printer:String.escaped

let assert_o_file_content_equal
    (name : string)
    (o_file : File.open_file)
    exp =
  name >:: fun _ -> assert_equal exp o_file.content.text

let make_test_add_line (name : string) str ind o_file exp =
  assert_o_file_content_equal name (File.add_line str ind o_file) exp

let make_test_delete_line (name : string) ind o_file exp =
  assert_o_file_content_equal name (File.delete_line ind o_file) exp

let make_test_combine_lines (name : string) ind_dest ind_src o_file exp
    =
  assert_o_file_content_equal name
    (File.combine_lines ind_dest ind_src o_file)
    exp

let make_test_split_line (name : string) x_pos y_pos o_file exp =
  assert_o_file_content_equal name
    (File.split_line x_pos y_pos o_file)
    exp

let make_test_append_line (name : string) str o_file exp =
  assert_o_file_content_equal name (File.append_line str o_file) exp

let make_test_append_to_line (name : string) str ind o_file exp =
  assert_o_file_content_equal name
    (File.append_to_line str ind o_file)
    exp

let make_test_add_to_line (name : string) str x_pos y_pos o_file exp =
  assert_o_file_content_equal name
    (File.add_to_line str x_pos y_pos o_file)
    exp

let make_test_delete_char_from_line
    (name : string)
    x_pos
    y_pos
    o_file
    exp =
  assert_o_file_content_equal name
    (File.delete_char_from_line x_pos y_pos o_file)
    exp

let make_test_replace_string_file (name : string) orig final o_file exp
    =
  assert_o_file_content_equal name
    (File.replace_string_file orig final o_file)
    exp

(* This tail-recursive map is necessary because the regular List.map
   generates a stack overflow when mapping the words in the huge
   Dictionary that I'm using*)
let tail_rec_map f l =
  let rec map_aux acc = function
    | [] -> List.rev acc
    | x :: xs ->
        map_aux
          begin
            f x :: acc
          end xs
  in
  map_aux [] l

let make_dict words =
  let open Predict in
  List.fold_left
    begin
      fun acc word ->
      acc |> insert word
    end
    empty words

let webster =
  Yojson.Basic.from_file "src/words.json"
  |> Yojson.Basic.Util.to_assoc |> tail_rec_map fst

let dict = make_dict webster
let dict1 = Predict.(empty |> insert "animal")

let predict_suite =
  [
    make_test_predict "an inserted word should have no completion" ""
      Predict.(dict1 |> find "animal");
    make_test_predict "anim should complete to animal here" "al"
      Predict.(dict1 |> find "anim");
    make_test_predict "case should not matter" ""
      Predict.(dict1 |> find "AniMAl");
    make_test_predict
      "ca should complete with no suffix (ca is california)" ""
      Predict.(dict |> find "ca");
    make_test_predict "armadi should complete with suffix llo" "llo"
      Predict.(dict |> find "armadi");
    make_test_predict "bonj should complete with suffix our" "our"
      Predict.(dict |> find "bonj");
    make_test_predict "constru should complete with suffix e" "e"
      Predict.(dict |> find "constru");
    make_test_predict "questi should complete with suffix on" "on"
      Predict.(dict |> find "questi");
    make_test_predict "croiss should complete with suffix ant" "ant"
      Predict.(dict |> find "croiss");
    make_test_predict "hallm should complete with suffix an" "an"
      Predict.(dict |> find "hallm");
    make_test_predict "constru should complete with suffix e" "e"
      Predict.(dict |> find "constru");
    make_test_predict "cloaki should complete with suffix ng" "ng"
      Predict.(dict |> find "cloaki");
    make_test_predict "rainb should complete with suffix ow" "ow"
      Predict.(dict |> find "rainb");
    make_test_predict "counterprod should complete with suffix uctive"
      "uctive"
      Predict.(dict |> find "counterprod");
    make_test_predict "knis should complete with suffix h" "h"
      Predict.(dict |> find "knis");
    make_test_predict "knig should complete with suffix ht" "ht"
      Predict.(dict |> find "knig");
    make_test_predict "revoluti should complete with suffix on" "on"
      Predict.(dict |> find "revoluti");
    make_test_predict "neand should complete with suffix erthal"
      "erthal"
      Predict.(dict |> find "neand");
    make_test_predict "cloaki should complete with suffix ng" "ng"
      Predict.(dict |> find "cloaki");
    make_test_predict "dinosa should complete with suffix ur" "ur"
      Predict.(dict |> find "dinosa");
    make_test_predict "tricera should complete with suffix tops" "tops"
      Predict.(dict |> find "tricera");
    make_test_predict "rhinoc should complete with suffix eri" "eri"
      Predict.(dict |> find "rhinoc");
    make_test_predict "kindr should complete with suffix ed" "ed"
      Predict.(dict |> find "kindr");
    make_test_predict "diopha should complete with suffix ntine" "ntine"
      Predict.(dict |> find "diopha");
    make_test_predict "daunti should complete with suffix ng" "ng"
      Predict.(dict |> find "daunti");
    make_test_predict "connivi should complete with suffix ng" "ng"
      Predict.(dict |> find "connivi");
    make_test_predict "esot should complete with suffix ery" "ery"
      Predict.(dict |> find "esot");
    make_test_predict "niaga should complete with suffix ra" "ra"
      Predict.(dict |> find "niaga");
    make_test_predict "supercallo should complete with suffix sal" "sal"
      Predict.(dict |> find "supercallo");
    make_test_predict "veritab should complete with suffix ly" "ly"
      Predict.(dict |> find "veritab");
    make_test_predict "cloaki should complete with suffix ng" "ng"
      Predict.(dict |> find "cloaki");
    make_test_predict "flou should complete with suffix t" "t"
      Predict.(dict |> find "flou");
    make_test_predict "becham should complete with suffix el" "el"
      Predict.(dict |> find "becham");
    make_test_predict "hyperch should complete with suffix olia" "olia"
      Predict.(dict |> find "hyperch");
    make_test_predict "ambu should complete with suffix sh" "sh"
      Predict.(dict |> find "ambu");
    make_test_predict "anbu should complete with suffix ry" "ry"
      Predict.(dict |> find "anbu");
  ]

let test_o_file = (File.load_from_file "test/test_file.txt").open_file

let test_short_o_file =
  (File.load_from_file "test/test_file_shortened.txt").open_file

let o_file_string_suite =
  [
    make_test_split_string
      "Splitting 'Lorem ipsum dolor est' at index 0 returns ('', \
       'Lorem ipsum dolor est')."
      "Lorem ipsum dolor est" 0
      ("", "Lorem ipsum dolor est");
    make_test_split_string
      "Splitting 'Lorem ipsum dolor est' at index 21 returns ('Lorem \
       ipsum dolor est', '')."
      "Lorem ipsum dolor est" 21
      ("Lorem ipsum dolor est", "");
    make_test_split_string
      "Splitting 'Lorem ipsum dolor est' at index 1 returns ('L', \
       'orem ipsum dolor est', '')."
      "Lorem ipsum dolor est" 1
      ("L", "orem ipsum dolor est");
    make_test_split_string
      "Splitting 'Lorem ipsum dolor est' at index 5 returns ('Lorem', \
       ' ipsum dolor est', '')."
      "Lorem ipsum dolor est" 5
      ("Lorem", " ipsum dolor est");
    make_test_delete_backwards_string
      "Deleting with size 0 at index 1 in 'Lorem ipsum dolor est' \
       returns 'Lorem ipsum dolor est'"
      "Lorem ipsum dolor est" 1 0 "Lorem ipsum dolor est";
    make_test_delete_backwards_string
      "Deleting with size 1 at index 1 in 'Lorem ipsum dolor est' \
       returns 'orem ipsum dolor est'"
      "Lorem ipsum dolor est" 1 1 "orem ipsum dolor est";
    make_test_delete_backwards_string
      "Deleting with size 1 at index 21 in 'Lorem ipsum dolor est' \
       returns 'Lorem ipsum dolor es'"
      "Lorem ipsum dolor est" 21 1 "Lorem ipsum dolor es";
    make_test_delete_backwards_string
      "Deleting with size 4 at index 11 in 'Lorem ipsum dolor est' \
       returns 'Lorem i dolor est'"
      "Lorem ipsum dolor est" 11 4 "Lorem i dolor est";
    make_test_delete_backwards_string
      "Deleting with size 21 at index 21 in 'Lorem ipsum dolor est' \
       returns ''"
      "Lorem ipsum dolor est" 21 21 "";
    make_test_delete_backwards_string
      "Deleting with size 20 at index 21 in 'Lorem ipsum dolor est' \
       returns 'L'"
      "Lorem ipsum dolor est" 21 20 "L";
    make_test_add_string
      "Adding string '' at index 5 in 'Lorem ipsum dolor est' returns \
       'Lorem ipsum dolor est'"
      "Lorem ipsum dolor est" "" 5 "Lorem ipsum dolor est";
    make_test_add_string
      "Adding string '3110' at index 5 in 'Lorem ipsum dolor est' \
       returns 'Lorem3110 ipsum dolor est'"
      "Lorem ipsum dolor est" "3110" 5 "Lorem3110 ipsum dolor est";
    make_test_add_string
      "Adding string '3110' at index 0 in 'Lorem ipsum dolor est' \
       returns '3110Lorem ipsum dolor est'"
      "Lorem ipsum dolor est" "3110" 0 "3110Lorem ipsum dolor est";
    make_test_add_string
      "Adding string '3110' at index 21 in 'Lorem ipsum dolor est' \
       returns 'Lorem ipsum dolor est3110'"
      "Lorem ipsum dolor est" "3110" 21 "Lorem ipsum dolor est3110";
    make_test_replace_string
      "Replacing string 'Lorem' with '3110' in 'Lorem ipsum dolor est' \
       returns '3110 ipsum dolor est'"
      "Lorem" "3110" "Lorem ipsum dolor est" "3110 ipsum dolor est";
    make_test_replace_string
      "Replacing string 'm' with '3110' in 'Lorem ipsum dolor est' \
       returns 'Lore3110 ipsu3110 dolor est'"
      "m" "3110" "Lorem ipsum dolor est" "Lore3110 ipsu3110 dolor est";
    make_test_replace_string
      "Replacing string 'Lorum' with '3110' in 'Lorem ipsum dolor est' \
       returns 'Lorem ipsum dolor est'"
      "Lorum" "3110" "Lorem ipsum dolor est" "Lorem ipsum dolor est";
    make_test_replace_string
      "Replacing string 's' with 'stamp' in 'Lorem ipsum dolor est' \
       returns 'Lorem ipstampum dolor estampt'"
      "s" "stamp" "Lorem ipsum dolor est"
      "Lorem ipstampum dolor estampt";
    make_test_replace_string
      "Replacing string 'the' with 'bye' in 'one two three the them' \
       returns 'one two three bye byem'"
      "the" "bye" "one two three the them" "one two three bye byem";
    make_test_replace_string
      "Replacing string 't' with '1234567890' in 'tttt' returns \
       '1234567890123456789012345678901234567890'"
      "t" "1234567890" "tttt" "1234567890123456789012345678901234567890";
    make_test_raw_text
      "Raw text of 'test_file.txt' after loading and translation into \
       an open_file should return the same content as in the original \
       text file, but with newlines."
      test_o_file
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do \
       eiusmod tempor\n\
       incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
       veniam, quis \n\
       nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
       commodo consequat.\n\
       Duis aute irure dolor in reprehenderit in voluptate velit esse \
       cillum dolore eu\n\
       fugiat nulla pariatur. Excepteur sint occaecat cupidatat non \
       proident, sunt in\n\
       culpa qui officia deserunt mollit anim id est laborum.";
    make_test_raw_text
      "Raw text of 'test_file_shortened.txt' after loading and \
       translation into an open_file should return the same content as \
       in the original text file, but with newlines."
      test_short_o_file
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do \
       eiusmod tempor\n\
       incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
       veniam, quis";
  ]

let o_file_suite =
  [
    make_test_add_line
      "The string '3110' added to the line at index 1 for the \
       shortened test file results in the same text but with '3110' \
       between the first and second lines."
      "3110" 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "3110";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_add_line
      "The string '3110' added to the line at index 0 for the \
       shortened test file results in the same text but with '3110' \
       before the other two lines."
      "3110" 0 test_short_o_file
      [
        "3110";
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_add_line
      "The string '3110' added to the line at index 2 for the \
       shortened test file results in the same text but with '3110' \
       after the other two lines."
      "3110" 2 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
        "3110";
      ];
    make_test_delete_line
      "Removing the line at index 1 for the shortened test file should \
       return a string list with only the first line"
      1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
      ];
    make_test_delete_line
      "Removing the line at index 0 for the shortened test file should \
       return a string list with only the second line"
      0 test_short_o_file
      [
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_combine_lines
      "Combining the line at index 1 into the line at index 0 for the \
       shortened text file should result in a file with a single line \
       containing the text from line 1 then the text from line 2, in \
       that order"
      0 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod temporincididunt ut labore et dolore magna aliqua. \
         Ut enim ad minim veniam, quis";
      ];
    make_test_combine_lines
      "Combining the line at index 0 into the line at index 1 for the \
       shortened text file should result in a file with a single line \
       containing the text from line 2 then the text from line 1, in \
       that order"
      1 0 test_short_o_file
      [
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quisLorem ipsum dolor sit amet, consectetur \
         adipiscing elit, sed do eiusmod tempor";
      ];
    make_test_combine_lines
      "Combining the line at index 3 into the line at index 1 for the \
       normal test text file should result in a file with the fourth \
       line having been added to the end of the second line, and \
       thereby removed from its original place"
      1 3 test_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis Duis aute irure dolor in reprehenderit in \
         voluptate velit esse cillum dolore eu";
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
         commodo consequat.";
        "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non \
         proident, sunt in";
        "culpa qui officia deserunt mollit anim id est laborum.";
      ];
    make_test_combine_lines
      "Combining the line at index 0 into the line at index 5 for the \
       normal test text file should result in a file with the first \
       line having been added to the end of the last line, and thereby \
       removed from its original place"
      5 0 test_o_file
      [
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis ";
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
         commodo consequat.";
        "Duis aute irure dolor in reprehenderit in voluptate velit \
         esse cillum dolore eu";
        "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non \
         proident, sunt in";
        "culpa qui officia deserunt mollit anim id est laborum.Lorem \
         ipsum dolor sit amet, consectetur adipiscing elit, sed do \
         eiusmod tempor";
      ];
    make_test_combine_lines
      "Combining the line at index 5 into the line at index 0 for the \
       normal test text file should result in a file with the last \
       line having been added to the end of the first line, and \
       thereby removed from its original place"
      0 5 test_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod temporculpa qui officia deserunt mollit anim id \
         est laborum.";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis ";
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
         commodo consequat.";
        "Duis aute irure dolor in reprehenderit in voluptate velit \
         esse cillum dolore eu";
        "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non \
         proident, sunt in";
      ];
    make_test_split_line
      "Splitting the shortened text file at x_pos of 0 and y_pos of 0 \
       should result in the same text but with an added empty line at \
       the beginning"
      0 0 test_short_o_file
      [
        "";
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_split_line
      "Splitting the shortened text file at x_pos of 10 and y_pos of 0 \
       should result in the first line being split into two lines, the \
       first starting with 'Lorem' and ending with 'ipsu' and the \
       second starting with 'm dolor' and continuing to the end. The \
       second line, which is now the third, should remain unchanged"
      10 0 test_short_o_file
      [
        "Lorem ipsu";
        "m dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
         tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_split_line
      "Splitting the shortened text file at x_pos of 74 and y_pos of 1 \
       should result in a new empty string line after the first two \
       lines."
      74 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
        "";
      ];
    make_test_split_line
      "Splitting the shortened text file at x_pos of 78 and y_pos of 0 \
       should result in a new empty string line between the other two \
       lines."
      78 0 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_append_line
      "Appending the line '3110' to the end of the shortened file \
       should result in the new line '3110' being after the first two \
       lines of the file"
      "3110" test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
        "3110";
      ];
    make_test_append_line
      "Appending the line '' to the end of the shortened file should \
       result in the new empty line being after the first two lines of \
       the file"
      "" test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
        "";
      ];
    make_test_append_to_line
      "Appending the string '3110' to the end the first line should \
       result in the same text, but simply with a '3110' after \
       'tempor' in the first line."
      "3110" 0 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor3110";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_append_to_line
      "Appending the string '' to the end the first line should result \
       in the same text."
      "" 0 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_add_to_line
      "Adding the string '3110' at an x_pos of 0 and a y_pos of 0 in \
       the shortened file should result in the same file, but with \
       '3110' prepended to the first line"
      "3110" 0 0 test_short_o_file
      [
        "3110Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
         sed do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_add_to_line
      "Adding the string '3110' at an x_pos of 74 and a y_pos of 1 in \
       the shortened file should result in the same file, but with \
       '3110' appended to the second line"
      "3110" 74 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis3110";
      ];
    make_test_add_to_line
      "Adding the string '3110' at an x_pos of 5 and a y_pos of 0 in \
       the shortened file should result in the same file, but with \
       '3110' added right after the 'Lorem' in the first line"
      "3110" 5 0 test_short_o_file
      [
        "Lorem3110 ipsum dolor sit amet, consectetur adipiscing elit, \
         sed do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_add_to_line
      "Adding the string '3110' at an x_pos of 78 and a y_pos of 0 in \
       the shortened file should result in the same file, but with \
       '3110' added right at the end of the first line"
      "3110" 78 0 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor3110";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_add_to_line
      "Adding the string '3110' at an x_pos of 0 and a y_pos of 1 in \
       the shortened file should result in the same file, but with \
       '3110' added right at the beginning of the second line"
      "3110" 0 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "3110incididunt ut labore et dolore magna aliqua. Ut enim ad \
         minim veniam, quis";
      ];
    make_test_add_to_line
      "Adding the string '' at an x_pos of 15 and a y_pos of 1 in the \
       shortened file should result in the same file"
      "" 15 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_delete_char_from_line
      "Deleting the character behind an x_pos of 0 and a y_pos of 0 \
       from the shortened file should return the same shortened file"
      0 0 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_delete_char_from_line
      "Deleting the character behind an x_pos of 0 and a y_pos of 1 \
       from the shortened file should return a file with a single line \
       that combines the two lines, such that the first line is placed \
       right before the second line"
      0 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod temporincididunt ut labore et dolore magna aliqua. \
         Ut enim ad minim veniam, quis";
      ];
    make_test_delete_char_from_line
      "Deleting the character behind an x_pos of 1 and a y_pos of 0 \
       from the shortened file should return the same shortened file, \
       except now the first line is missing its first letter"
      1 0 test_short_o_file
      [
        "orem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_delete_char_from_line
      "Deleting the character behind an x_pos of 5 and a y_pos of 0 \
       from the shortened file should return the same shortened file, \
       except now the 'Lorem' in the first line is now 'Lore'"
      5 0 test_short_o_file
      [
        "Lore ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_delete_char_from_line
      "Deleting the character behind an x_pos of 1 and a y_pos of 1 \
       from the shortened file should return the same shortened file, \
       except now the second line is missing its first letter"
      1 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "ncididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_delete_char_from_line
      "Deleting the character behind an x_pos of 74 and a y_pos of 1 \
       from the shortened file should return the same shortened file, \
       except now the second line is missing its last letter"
      74 1 test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, qui";
      ];
    make_test_replace_string_file
      "Replacing every instance of the string 'Lorem' in the shortened \
       file with '3110' should result in the same file but with the \
       first word being '3110' instead"
      "Lorem" "3110" test_short_o_file
      [
        "3110 ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_replace_string_file
      "Replacing every instance of the string 'xyxyx' in the shortened \
       file with '3110' should result in the same file"
      "xyxyx" "3110" test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_replace_string_file
      "Replacing every instance of the string 'dolor' in the shortened \
       file with '3110' should result in the same file, but with \
       'dolor' in line one exchanged for '3110', and 'dolore' in line \
       two exchanged for '3110e'"
      "dolor" "3110" test_short_o_file
      [
        "Lorem ipsum 3110 sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et 3110e magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_replace_string_file
      "Replacing every instance of the string 'ipsum' in the shortened \
       file with '' should result in the same file but with the word \
       'ipsum' missing in the first line, and two spaces there instead"
      "ipsum" "" test_short_o_file
      [
        "Lorem  dolor sit amet, consectetur adipiscing elit, sed do \
         eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_replace_string_file
      "Replacing every instance of the string 'Lorem ipsum dolor sit \
       amet, consectetur adipiscing elit, sed do eiusmod tempor' in \
       the shortened file with 'Lorem ipsum dolor sit amet, \
       consectetur adipiscing elit, sed do eiusmod tempor' should \
       result in the same file"
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do \
       eiusmod tempor"
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do \
       eiusmod tempor"
      test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_replace_string_file
      "Replacing every instance of the string 't' in the shortened \
       file with 'tt' should result in the same file, but with every \
       instance of 't' doubled"
      "t" "tt" test_short_o_file
      [
        "Lorem ipsum dolor sitt amett, consecttettur adipiscing elitt, \
         sed do eiusmod ttempor";
        "incididuntt utt labore ett dolore magna aliqua. Utt enim ad \
         minim veniam, quis";
      ];
    make_test_replace_string_file
      "Replacing every instance of the string 'tempor incididunt' in \
       the shortened file with '' should result in the same file"
      "tempor incidiunt" "" test_short_o_file
      [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
         do eiusmod tempor";
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
         veniam, quis";
      ];
    make_test_get_word "The word at (5, 0) for the test file is 'Lorem'"
      5 0 test_o_file "Lorem";
    make_test_get_word
      "The word at (11, 0) for the test file is 'ipsum'" 11 0
      test_o_file "ipsum";
    make_test_get_word
      "The word at (78, 0) for the test file is 'tempor'" 78 0
      test_o_file "tempor";
    make_test_get_word
      "The word at (79, 0) for the test file is 'tempor'" 79 0
      test_o_file "tempor";
    make_test_get_word
      "The word at (4, 0) for the test file is invalid, and therefore \
       returns an empty string."
      4 0 test_o_file "";
    make_test_get_word
      "The word at (10, 1) for the test file is 'incididunt'." 10 1
      test_o_file "incididunt";
  ]

let suite =
  "Vortex test suite"
  >::: List.flatten [ predict_suite; o_file_string_suite; o_file_suite ]

let _ = run_test_tt_main suite