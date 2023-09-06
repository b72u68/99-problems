(* Problem 1 *)
let rec last = function
  | [] -> None
  | [ h ] -> Some h
  | _ :: t -> last t
;;

(* Problem 2 *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ h1; h2 ] -> Some (h1, h2)
  | _ :: t -> last_two t
;;

(* Problem 3 *)
let rec nth l n =
  match l, n with
  | [], _ -> raise (Failure "nth")
  | h :: _, 0 -> h
  | _ :: t, n -> nth t (n - 1)
;;

(* Problem 4 *)
let length = List.fold_left (fun acc _ -> acc + 1) 0

(* Problem 5 *)
let rev = List.fold_left (fun acc h -> h :: acc) []

(* Problem 6 *)
let is_palindrome l = l = rev l

(* Problem 7 *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten l =
  List.fold_left
    (fun acc x ->
      match x with
      | One x -> acc @ [ x ]
      | Many xs -> acc @ flatten xs)
    []
    l
;;

(* Problem 8 *)
let rec compress = function
  | [] -> []
  | [ h ] -> [ h ]
  | h1 :: h2 :: t -> if h1 = h2 then compress (h2 :: t) else h1 :: compress (h2 :: t)
;;

let () =
  let _ = assert (last [ "a"; "b"; "c"; "d" ] = Some "d") in
  let _ = assert (last [] = None) in
  let _ = assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")) in
  let _ = assert (last_two [ "a" ] = None) in
  let _ = assert (last_two [] = None) in
  let _ = assert (nth [ "a"; "b"; "c"; "d" ] 2 = "c") in
  let _ =
    try
      let _ = nth [ "a" ] 2 in
      ()
    with
    | Failure _ -> assert true
  in
  let _ = assert (length [ "a"; "b"; "c" ] = 3) in
  let _ = assert (length [] = 0) in
  let _ = assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]) in
  let _ = assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ] = true) in
  let _ = assert (is_palindrome [ "a"; "b" ] = false) in
  let _ =
    assert (
      flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
      = [ "a"; "b"; "c"; "d"; "e" ])
  in
  let _ =
    assert (
      compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      = [ "a"; "b"; "c"; "a"; "d"; "e" ])
  in
  ()
;;
