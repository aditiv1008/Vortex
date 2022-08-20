exception InvalidIndex

val insert : 'a -> int -> 'a list -> 'a list
(** [insert x ind list] inserts element [x] before index [ind] in
    [list]. Raises [InvalidIndex] if [ind] < 0 or [ind] > the length of
    [list]. *)

val remove : int -> 'a list -> 'a list
(** [remove ind list] removes the element in the list [list] at index
    [ind]. Raises [InvalidIndex] if [ind] < 0 or [ind] > the length of
    [list]. *)

val choose : bool -> 'a -> 'a -> 'a
(** [choose c a b] returns [a] if [c] is true, otherwise [b]. This was
    only implemented for readability and saving space. *)

val tail_rec_map : ('a -> 'b) -> 'a list -> 'b list
(** [tail_rec_map f \[x1; x2; ... ; xn\]] returns
    [f x1; f x2; ... ; f xn]. It uses tail_recursion. *)

val make_dict : string list -> Predict.t
(** [make_dict words] returns the prefix trie created by inserting each
    word in words sequentially into an empty prefix trie. *)
