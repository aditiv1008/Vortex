type node = {
  c : char;
  is_word : bool;
  children : t;
}

and t = node list

let empty = []

let is_latin c =
  let n = Char.code c in
  let a = Char.code 'a' in
  let aa = Char.code 'A' in
  begin
    begin
      n - a >= 0
      && n - a < 26
    end
    || begin
         n - aa >= 0
         && n - aa < 26
       end
  end

let is_regular s =
  if s = "" then true
  else
    let len = String.length s in
    let rec good_until s pos =
      if pos > len - 1 then true
      else
        match is_latin s.[pos] with
        | true -> good_until s (pos + 1)
        | false -> false
    in
    good_until s 0

let string_to_char_list s =
  let rec helper i cs =
    if i < 0 then cs
    else
      helper
        begin
          i - 1
        end
        begin
          Char.lowercase_ascii s.[i]
          :: cs
        end
  in
  helper
    begin
      String.length s - 1
    end
    []

let char_list_to_string cs =
  let buf = Buffer.create (List.length cs) in
  List.iter (Buffer.add_char buf) cs;
  Buffer.contents buf

let rec insert_helper cs tr =
  match cs with
  | [] -> tr
  | h :: t ->
      let nd =
        try
          List.find
            begin
              fun n ->
              n.c = h
            end
            tr
        with Not_found -> { c = h; is_word = false; children = [] }
      in
      {
        c = nd.c;
        is_word = t = [] || nd.is_word;
        children = insert_helper t nd.children;
      }
      :: List.filter
           begin
             fun x ->
             x.c != h
           end
           tr

let insert s tr =
  if not (is_regular s) then tr
  else
    let cs = string_to_char_list s in
    insert_helper cs tr

let bfs tr =
  let q = Queue.create () in
  tr |> List.iter (fun n -> q |> Queue.push (n, [ n.c ]));
  let ans = ref [] in
  let looking = ref true in
  while
    begin
      (not (Queue.is_empty q))
      && !looking
    end
  do
    let nd, cs = Queue.pop q in
    if nd.is_word then begin
      ans := List.rev cs;
      looking := false
    end
    else
      begin
        nd.children
        |> List.iter (fun n -> q |> Queue.push (n, n.c :: cs))
      end
  done;
  char_list_to_string !ans

let rec find_helper cs tr =
  match cs with
  | [] -> failwith "never reached"
  | h :: t -> begin
      let nd_opt =
        List.find_opt
          begin
            fun n ->
            n.c = h
          end
          tr
      in
      match nd_opt with
      | Some nd -> begin
          match t with
          | [] -> if nd.is_word then "" else bfs nd.children
          | tt -> find_helper tt nd.children
        end
      | None -> ""
    end

let find s tr =
  if s = "" then ""
  else if not (is_regular s) then ""
  else
    let cs = string_to_char_list s in
    find_helper cs tr