type t
(** t is the abstract type representing a prefix trie. *)

val empty : t
(** [empty] is an empty prefix trie with no words. *)

val insert : string -> t -> t
(** [insert s tr] returns a new prefix trie with all the prefixes from
    tr and [s] as well. *)

val find : string -> t -> string
(** [find s tr] returns the suffix of the shortest word in [tr] that
    matches the prefix [s]. *)
