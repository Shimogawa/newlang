open Elements
module S = Set.Make (String)

let split_list n (xs : 'a list) =
  let rec split n (h : 'a list) (t : 'a list) =
    match t with
    | [] -> (h, t)
    | x :: xs -> if n = 0 then (h, t) else split (n - 1) (h @ [ x ]) xs
  in
  split n [] xs

let zip_to_hashtbl ks vs =
  try List.combine ks vs |> List.to_seq |> Hashtbl.of_seq
  with Invalid_argument _ -> fail "argument count not matched"

let utf8_string_len s =
  let de = Uutf.decoder ~encoding:`UTF_8 (`String s)
  and quit_loop = ref false in
  while not !quit_loop do
    match Uutf.decode de with
    | `End -> quit_loop := true
    | `Uchar _ -> ()
    | _ -> fail "unable to decode string"
  done;
  Uutf.decoder_count de
