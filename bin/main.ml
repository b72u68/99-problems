(* SOLUTION *)

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

let compress_fr list =
  List.fold_right
    (fun x acc ->
      match acc with
      | [] -> [ x ]
      | h :: _ -> if h = x then acc else x :: acc)
    list
    []
;;

(* Problem 9 *)
let pack list =
  let aux acc x =
    match acc with
    | [] -> []
    | h :: t ->
      (try if List.hd h = x then (x :: h) :: t else [ x ] :: acc with
       | Failure _ -> [ x ] :: t)
  in
  list |> List.fold_left aux [ [] ] |> List.rev
;;

(* Problem 10 *)
let encode list =
  List.fold_right
    (fun x acc ->
      match acc with
      | [] -> [ 1, x ]
      | (c, x') :: t -> if x' = x then (c + 1, x) :: t else (1, x) :: acc)
    list
    []
;;

(* Problem 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_rle list =
  List.fold_right
    (fun x acc ->
      match acc with
      | [] -> [ One x ]
      | One x' :: t -> if x' = x then Many (2, x) :: t else One x :: acc
      | Many (c, x') :: t -> if x' = x then Many (c + 1, x) :: t else One x :: acc)
    list
    []
;;

(* Problem 12 *)
let rec decode = function
  | [] -> []
  | One x :: t -> x :: decode t
  | Many (c, x) :: t -> if c = 0 then decode t else x :: decode (Many (c - 1, x) :: t)
;;

(* Problem 13 *)
let encode_dir =
  let rle_count count x = if count = 1 then One x else Many (count, x) in
  let rec aux count = function
    | [] -> []
    | [ h ] -> [ rle_count (count + 1) h ]
    | h1 :: (h2 :: _ as t) ->
      if h1 = h2 then aux (count + 1) t else rle_count (count + 1) h1 :: aux 0 t
  in
  aux 0
;;

(* Problem 14 *)
let duplicate list = List.fold_right (fun x acc -> x :: x :: acc) list []

(* Problem 15 *)
let replicate list n =
  let rec rep x n = if n = 0 then [] else x :: rep x (n - 1) in
  List.fold_right (fun x acc -> rep x n @ acc) list []
;;

(* Problem 16 *)
let drop list n =
  let rec aux = function
    | [], _ -> []
    | _ :: t, 1 -> aux (t, n)
    | h :: t, n -> h :: aux (t, n - 1)
  in
  aux (list, n)
;;

(* Problem 17 *)
let split list n =
  let rec aux acc = function
    | [], _ -> acc, []
    | list, 0 -> acc, list
    | h :: t, n -> aux (acc @ [ h ]) (t, n - 1)
  in
  aux [] (list, n)
;;

(* TESTING *)
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
  let _ =
    assert (
      compress_fr [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      = [ "a"; "b"; "c"; "a"; "d"; "e" ])
  in
  let _ =
    assert (
      pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
      = [ [ "a"; "a"; "a"; "a" ]
        ; [ "b" ]
        ; [ "c"; "c" ]
        ; [ "a"; "a" ]
        ; [ "d"; "d" ]
        ; [ "e"; "e"; "e"; "e" ]
        ])
  in
  let _ =
    assert (
      encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ])
  in
  let _ =
    assert (
      encode_rle [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ])
  in
  let _ =
    assert (
      decode
        [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
      = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
  in
  let _ =
    assert (
      encode_dir [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ])
  in
  let _ =
    assert (
      duplicate [ "a"; "b"; "c"; "c"; "d" ]
      = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ])
  in
  let _ =
    assert (
      replicate [ "a"; "b"; "c" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ])
  in
  let _ =
    assert (
      drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
      = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ])
  in
  let _ =
    assert (
      split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
      = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]))
  in
  let _ = assert (split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], [])) in
  ()
;;
