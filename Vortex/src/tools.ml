exception InvalidIndex

let rec insert_helper x ind list : 'a list =
  if ind = 0 then x :: list
  else
    match list with
    | [] -> raise InvalidIndex
    | h :: t ->
        begin
          h
          :: insert_helper x (ind - 1) t
        end

let insert x ind list : 'a list =
  if ind < 0 then raise InvalidIndex else insert_helper x ind list

let rec remove_helper ind list : 'a list =
  match list with
  | [] -> raise InvalidIndex
  | h :: t ->
      if ind = 0 then t
      else
        begin
          h
          :: remove_helper (ind - 1) t
        end

let remove ind list : 'a list =
  if ind < 0 then raise InvalidIndex else remove_helper ind list

(* This is implemented in order to increase readibility. *)
let choose c a b : 'a = if c then a else b

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
