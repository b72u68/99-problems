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

(* Problem 18 *)
let slice list s e = fst (split (snd (split list s)) (e - s + 1))

(* Problem 19 *)
let rotate list n =
  let l1, l2 = split list n in
  l2 @ l1
;;

(* Problem 20 *)
let rec remove_at n list =
  if n = 0 then List.tl list else List.hd list :: remove_at (n - 1) (List.tl list)
;;

(* Problem 21 *)
let rec insert_at x idx list =
  match list with
  | [] -> [ x ]
  | h :: t -> if idx = 0 then x :: list else h :: insert_at x (idx - 1) t
;;

(* Problem 22 *)
let range s e =
  let rec aux s e = if s = e then [ e ] else s :: aux (s + 1) e in
  if s > e then List.rev (aux e s) else aux s e
;;

(* Problem 23 *)
let rand_select list n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then h, acc @ t else extract (h :: acc) (n - 1) t
  in
  let extract_rand list = extract [] (Random.int (List.length list)) list in
  let rec aux n acc list =
    if n = 0
    then acc
    else (
      let picked, rest = extract_rand list in
      aux (n - 1) (picked :: acc) rest)
  in
  aux (min n (List.length list)) [] list
;;

(* Problem 24 *)
let lotto_select n m = rand_select (range 1 m) n

(* Problem 25 *)

(* Problem 26 *)
let rec extract n list =
  if n <= 0
  then [ [] ]
  else (
    match list with
    | [] -> []
    | h :: t ->
      let with_h = List.map (fun l -> h :: l) (extract (n - 1) t) in
      let without_h = extract n t in
      with_h @ without_h)
;;

(* Problem 27 *)
let rec group list sizes =
  let filter_elem x = List.filter (( <> ) x) in
  let rec extract n list =
    if n <= 0
    then [ [], list ]
    else (
      match list with
      | [] -> []
      | h :: t ->
        let with_h =
          List.map (fun (l, rest) -> h :: l, filter_elem h rest) (extract (n - 1) t)
        in
        let without_h =
          List.map (fun (l, rest) -> l, h :: rest) (extract n (filter_elem h t))
        in
        with_h @ without_h)
  in
  match sizes with
  | [] -> [ [] ]
  | n :: t ->
    List.fold_left
      ( @ )
      []
      (List.map
         (fun (l, rest) -> List.map (fun gl -> l :: gl) (group rest t))
         (extract n list))
;;

(* Problem 28 *)

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
  let _ =
    assert (
      slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
      = [ "c"; "d"; "e"; "f"; "g" ])
  in
  let _ =
    assert (
      rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
      = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ])
  in
  let _ = assert (remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ]) in
  let _ = assert (remove_at 0 [ "a"; "b"; "c"; "d" ] = [ "b"; "c"; "d" ]) in
  let _ =
    assert (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "alfa"; "b"; "c"; "d" ])
  in
  let _ =
    assert (insert_at "alfa" 0 [ "a"; "b"; "c"; "d" ] = [ "alfa"; "a"; "b"; "c"; "d" ])
  in
  let _ = assert (range 4 9 = [ 4; 5; 6; 7; 8; 9 ]) in
  let _ = assert (range 9 4 = [ 9; 8; 7; 6; 5; 4 ]) in
  let _ = rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3 in
  let _ = lotto_select 2 3 in
  let _ =
    assert (
      extract 2 [ "a"; "b"; "c"; "d" ]
      = [ [ "a"; "b" ]
        ; [ "a"; "c" ]
        ; [ "a"; "d" ]
        ; [ "b"; "c" ]
        ; [ "b"; "d" ]
        ; [ "c"; "d" ]
        ])
  in
  let _ =
    assert (
      group [ "a"; "b"; "c"; "d" ] [ 2; 1 ]
      = [ [ [ "a"; "b" ]; [ "c" ] ]
        ; [ [ "a"; "b" ]; [ "d" ] ]
        ; [ [ "a"; "c" ]; [ "b" ] ]
        ; [ [ "a"; "c" ]; [ "d" ] ]
        ; [ [ "a"; "d" ]; [ "b" ] ]
        ; [ [ "a"; "d" ]; [ "c" ] ]
        ; [ [ "b"; "c" ]; [ "a" ] ]
        ; [ [ "b"; "c" ]; [ "d" ] ]
        ; [ [ "b"; "d" ]; [ "a" ] ]
        ; [ [ "b"; "d" ]; [ "c" ] ]
        ; [ [ "c"; "d" ]; [ "a" ] ]
        ; [ [ "c"; "d" ]; [ "b" ] ]
        ])
  in
  ()
;;
