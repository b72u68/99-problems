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

(* Problem 28-30 *)
let rec insert cmp e = function
  | [] -> [ e ]
  | h :: t as l -> if cmp e h <= 0 then e :: l else h :: insert cmp e t
;;

let rec sort cmp = function
  | [] -> []
  | h :: t -> insert cmp h (sort cmp t)
;;

let length_sort = sort List.compare_lengths

let frequency_sort list =
  List.map
    (fun l ->
      ( l
      , List.fold_left
          (fun acc l' -> if List.compare_lengths l l' = 0 then acc + 1 else acc)
          0
          list ))
    list
  |> sort (fun (_, f1) (_, f2) -> compare f1 f2)
  |> List.map fst
;;

(* Problem 31 *)
let is_prime n =
  let n = abs n in
  let rec aux i =
    if i * i > n then true else if n mod i = 0 then false else aux (i + 1)
  in
  if n <= 1 then false else aux 2
;;

let sieve n =
  let n = abs n in
  let filter_mul p = List.filter (fun n -> n mod p <> 0) in
  let rec aux = function
    | [] -> false
    | [ n' ] -> n' = n
    | p :: t -> aux (filter_mul p t)
  in
  n > 1 && (aux @@ range 2 n)
;;

(* Problem 32 *)
let rec gcd a b =
  if a = b
  then a
  else (
    let mi = min a b in
    let ma = max a b in
    gcd (ma - mi) mi)
;;

(* Problem 33 *)
let coprime a b = gcd a b = 1

(* Problem 34 *)
let phi n =
  List.fold_left (fun acc m -> if coprime n m then acc + 1 else acc) 0 (range 1 n)
;;

(* Problem 35 *)
let rec factors n =
  let rec aux d = if n mod d = 0 then d :: factors (n / d) else aux (d + 1) in
  if n <= 1 then [] else aux 2
;;

(* Problem 36 *)
let factors_enc n = List.map (fun (f, s) -> s, f) (encode @@ factors n)

(* Problem 37 *)
let phi_improved n =
  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n -> n * pow a (n - 1)
  in
  List.fold_left (fun acc (p, m) -> acc * (p - 1) * pow p (m - 1)) 1 (factors_enc n)
;;

(* Problem 38 *)
let timeit f a =
  let t0 = Unix.gettimeofday () in
  ignore (f a);
  let t1 = Unix.gettimeofday () in
  t1 -. t0
;;

(* Problem 39 *)
let rec all_primes s e =
  let filter_mul p = List.filter (fun n -> n mod p <> 0) in
  let rec aux acc = function
    | [] -> List.rev acc
    | p :: t -> aux (p :: acc) (filter_mul p t)
  in
  if is_prime s then aux [] @@ range s e else all_primes (s + 1) e
;;

(* Problem 40 *)
let goldbach n =
  let rec aux = function
    | [] -> raise Not_found
    | p :: t -> if is_prime (n - p) then p, n - p else aux t
  in
  aux @@ all_primes 2 ((n / 2) + 1)
;;

(* Problem 41 *)
let rec goldbach_list s e =
  if s > e
  then []
  else if s mod 2 = 1
  then goldbach_list (s + 1) e
  else (s, goldbach s) :: goldbach_list (s + 2) e
;;

(* Problem 46 *)
type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let eval_var ctx var = snd @@ List.find (fun (var', _) -> var = var') ctx

let rec eval_expr ctx = function
  | Var v -> eval_var ctx v
  | Not e -> not @@ eval_expr ctx e
  | And (e1, e2) -> eval_expr ctx e1 && eval_expr ctx e2
  | Or (e1, e2) -> eval_expr ctx e1 || eval_expr ctx e2
;;

let rec gen_ctxs = function
  | [] -> [ [] ]
  | h :: t ->
    let ctx_rest = gen_ctxs t in
    let h_true = List.map (fun ctx -> (h, true) :: ctx) ctx_rest in
    let h_false = List.map (fun ctx -> (h, false) :: ctx) ctx_rest in
    h_true @ h_false
;;

let table2 v1 v2 expr =
  let ctxs = [ true, true; true, false; false, true; false, false ] in
  List.map (fun (val1, val2) -> val1, val2, eval_expr [ v1, val1; v2, val2 ] expr) ctxs
;;

(* Problem 48 *)
let table vars expr = List.map (fun ctx -> ctx, eval_expr ctx expr) @@ gen_ctxs vars

(* Problem 49 *)
let gray n =
  let rec gray_next_level k l =
    if k < n
    then (
      let first_half, second_half =
        List.fold_left
          (fun (acc1, acc2) x -> ("0" ^ x) :: acc1, ("1" ^ x) :: acc2)
          ([], [])
          l
      in
      gray_next_level (k + 1) (List.rev_append first_half second_half))
    else l
  in
  gray_next_level 1 [ "0"; "1" ]
;;

(* Problem 50 *)
type hf_tree =
  | Node of hf_tree * hf_tree
  | Leaf of string

let huffman fs =
  let fsts = List.map (fun (s, f) -> Leaf s, f) fs in
  let rec construct_tree fst =
    let fsts' = sort (fun (_, f1) (_, f2) -> compare f1 f2) fst in
    match fsts' with
    | [] -> raise (Failure "Cannot construct Huffman tree")
    | [ (fst, _) ] -> fst
    | (t1, f1) :: (t2, f2) :: t -> construct_tree @@ ((Node (t1, t2), f1 + f2) :: t)
  in
  let rec decode_tree acc = function
    | Node (t1, t2) -> decode_tree (acc ^ "0") t1 @ decode_tree (acc ^ "1") t2
    | Leaf s -> [ s, acc ]
  in
  decode_tree "" @@ construct_tree fsts
;;

(* Problem 55 *)
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec cbal_tree n =
  if n = 0
  then [ Empty ]
  else if (n - 1) mod 2 = 0
  then (
    let ts = cbal_tree @@ ((n - 1) / 2) in
    List.fold_right
      ( @ )
      (List.map (fun t -> List.map (fun t' -> Node ('x', t, t')) ts) ts)
      [])
  else (
    let ts, ts' = cbal_tree @@ ((n - 1) / 2), cbal_tree @@ (((n - 1) / 2) + 1) in
    List.fold_right
      ( @ )
      (List.map (fun t -> List.map (fun t' -> Node ('x', t, t')) ts') ts)
      []
    @ List.fold_right
        ( @ )
        (List.map (fun t -> List.map (fun t' -> Node ('x', t, t')) ts) ts')
        [])
;;

(* Problem 56 *)
let rec is_mirror t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Empty, Node _ | Node _, Empty -> false
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror l2 r1
;;

let is_symmetric = function
  | Empty -> true
  | Node (_, t1, t2) -> is_mirror t1 t2
;;

(* Problem 57 *)
let construct =
  let rec insert_val v = function
    | Empty -> Node (v, Empty, Empty)
    | Node (v', lt, rt) ->
      if v' < v then Node (v', lt, insert_val v rt) else Node (v', insert_val v lt, rt)
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (insert_val h acc) t
  in
  aux Empty
;;

(* Problem 58 *)
let sym_cbal_trees n = List.filter is_symmetric (cbal_tree n)

(* Problem 59 *)
(* Build all trees with given [left] and [right] subtrees. *)
let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node ('x', l, r) :: a) all right
  in
  List.fold_left add_right_tree all left
;;

let rec hbal_tree n =
  if n = 0
  then [ Empty ]
  else if n = 1
  then [ Node ('x', Empty, Empty) ]
  else (
    let t1 = hbal_tree (n - 1) in
    let t2 = hbal_tree (n - 2) in
    add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 [])))
;;

(* Problem 60 *)
let rec min_nodes h =
  if h <= 0 then 0 else if h = 1 then 1 else min_nodes (h - 1) + min_nodes (h - 2) + 1
;;

let min_height n = int_of_float (ceil (log (float (n + 1)) /. log 2.))

(* Problem 61A *)
let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, lt, rt) -> count_leaves lt + count_leaves rt
;;

(* Problem 61B *)
let rec leaves = function
  | Empty -> []
  | Node (x, Empty, Empty) -> [ x ]
  | Node (_, lt, rt) -> leaves lt @ leaves rt
;;

(* Problem 62A *)
let rec internals = function
  | Empty -> []
  | Node (_, Empty, Empty) -> []
  | Node (x, lt, rt) -> [ x ] @ internals lt @ internals rt
;;

(* Problem 62B *)
let at_level t n =
  if n <= 0
  then []
  else (
    let rec aux lvl = function
      | Empty -> []
      | Node (x, lt, rt) -> if lvl = n then [ x ] else aux (lvl + 1) lt @ aux (lvl + 1) rt
    in
    aux 1 t)
;;

(* Problem 63 *)
let complete_binary_tree =
  let rec drop n = function
    | [] -> []
    | _ :: t as l -> if n <= 0 then l else drop (n - 1) t
  in
  let rec aux idx = function
    | [] -> Empty
    | h :: t ->
      let lt = aux (2 * idx) (drop (idx - 1) t) in
      let rt = aux ((2 * idx) + 1) (drop idx t) in
      Node (h, lt, rt)
  in
  aux 1
;;

(* Problem 64 *)
let layout_binary_tree_1 t =
  let rec layout depth x_left = function
    | Empty -> Empty, x_left
    | Node (v, l, r) ->
      let l', l_x_max = layout (depth + 1) x_left l in
      let r', r_x_max = layout (depth + 1) (l_x_max + 1) r in
      Node ((v, l_x_max, depth), l', r'), r_x_max
  in
  fst (layout 1 1 t)
;;

(* Problem 65 *)
(*let layout_binary_tree_2 t = ()*)

(* Problem 66 *)
(*let layout_binary_tree_3 t = ()*)

(* Problem 67 *)
let rec string_of_tree = function
  | Empty -> ""
  | Node (x, Empty, Empty) -> String.make 1 x
  | Node (x, l, r) ->
    String.make 1 x ^ "(" ^ string_of_tree l ^ "," ^ string_of_tree r ^ ")"
;;

let tree_of_string s =
  let rec make ofs s =
    if ofs >= String.length s || s.[ofs] = ',' || s.[ofs] = ')'
    then Empty, ofs
    else (
      let v = s.[ofs] in
      if ofs + 1 < String.length s && s.[ofs + 1] = '('
      then (
        let l, ofs = make (ofs + 2) s in
        let r, ofs = make (ofs + 1) s in
        Node (v, l, r), ofs + 1)
      else Node (v, Empty, Empty), ofs + 1)
  in
  fst (make 0 s)
;;

(* Problem 68 *)
let rec preorder = function
  | Empty -> []
  | Node (v, l, r) -> [ v ] @ preorder l @ preorder r
;;

let rec inorder = function
  | Empty -> []
  | Node (v, l, r) -> inorder l @ [ v ] @ inorder r
;;

(* Problem 69 *)
let rec dotstring_tree = function
  | Empty -> "."
  | Node (v, l, r) -> String.make 1 v ^ dotstring_tree l ^ dotstring_tree r
;;

let tree_dotstring s =
  let rec make ofs s =
    if ofs >= String.length s || s.[ofs] = '.'
    then Empty, ofs
    else (
      let v = s.[ofs] in
      let l, ofs = make (ofs + 1) s in
      let r, ofs = make (ofs + 1) s in
      Node (v, l, r), ofs)
  in
  fst (make 0 s)
;;

(* Problem 70A *)
type 'a mult_tree = T of 'a * 'a mult_tree list

let rec iter = function
  | [] -> "^"
  | h :: t -> string_of_mult_tree h ^ iter t

and string_of_mult_tree = function
  | T (v, ts) -> String.make 1 v ^ iter ts
;;

let mult_tree_of_string s =
  let rec loop ofs s =
    if ofs >= String.length s || s.[ofs] = '^'
    then [], ofs
    else (
      let ts, ofs = make ofs s in
      let ts', ofs = loop (ofs + 1) s in
      ts @ ts', ofs)
  and make ofs s =
    if ofs >= String.length s || s.[ofs] = '^'
    then [], ofs
    else (
      let v = s.[ofs] in
      let ts, ofs = loop (ofs + 1) s in
      [ T (v, ts) ], ofs)
  in
  List.nth (fst (make 0 s)) 0
;;

(* Problem 70B *)
let rec count_nodes = function
  | T (_, ts) -> 1 + List.fold_left (fun acc elem -> acc + count_nodes elem) 0 ts
;;

(* Problem 71 *)
let ipl =
  let rec ipl_sub len (T (_, sub)) =
    List.fold_left (fun acc t -> acc + ipl_sub (len + 1) t) len sub
  in
  ipl_sub 0
;;

(* Problem 72 *)
let rec bottom_up = function
  | T (v, sub) -> List.fold_right (fun t acc -> bottom_up t @ acc) sub [] @ [ v ]
;;

(* Problem 73 *)
let lispy t =
  let rec aux = function
    | T (v, sub) ->
      (match sub with
       | [] -> " " ^ String.make 1 v
       | _ ->
         " "
         ^ "("
         ^ String.make 1 v
         ^ List.fold_right (fun t acc -> aux t ^ acc) sub ""
         ^ ")")
  in
  let s = aux t in
  if s.[0] = ' ' then String.sub s 1 (String.length s - 1) else s
;;

(* Problem 80 *)
type 'a graph_term =
  { nodes : 'a list
  ; edges : ('a * 'a) list
  }

module SS = Set.Make (Char)

let gt_to_hf gt =
  let nodes = SS.of_list gt.nodes in
  let rec aux cur_nodes = function
    | [] ->
      List.fold_right
        (fun n acc -> " " ^ String.make 1 n ^ acc)
        (List.of_seq (SS.to_seq (SS.diff nodes cur_nodes)))
        ""
    | (n1, n2) :: t ->
      " "
      ^ String.make 1 n1
      ^ "-"
      ^ String.make 1 n2
      ^ aux (SS.add n2 (SS.add n1 cur_nodes)) t
  in
  let s = aux SS.empty gt.edges in
  String.sub s 1 (String.length s - 1)
;;

let hf_to_gt hf =
  let set_to_list s = List.of_seq (SS.to_seq s) in
  let rec aux nodes idx edges =
    if idx >= String.length hf
    then { nodes = set_to_list nodes; edges }
    else (
      let n = hf.[idx] in
      if idx + 1 >= String.length hf
      then { nodes = set_to_list (SS.add n nodes); edges }
      else if hf.[idx + 1] = ' '
      then aux (SS.add n nodes) (idx + 2) edges
      else if hf.[idx + 1] = '-'
      then (
        let n' = hf.[idx + 2] in
        aux (SS.add n' nodes) (idx + 4) ((n, n') :: edges))
      else raise (Failure (Printf.sprintf "Invalid token: %c" hf.[idx + 1])))
  in
  aux SS.empty 0 []
;;

(* Problem 81 *)
let rec neighbors node = function
  | [] -> []
  | (n, n') :: t ->
    if n = node
    then n' :: neighbors node t
    else if n' = node
    then n :: neighbors node t
    else neighbors node t
;;

let paths graph start_node end_node =
  let rec build_paths visited node =
    if SS.mem node visited
    then []
    else if node = end_node
    then [ [ node ] ]
    else (
      let next_nodes = neighbors node graph.edges in
      let visited = SS.add node visited in
      List.fold_left
        (fun acc n -> List.map (fun p -> node :: p) (build_paths visited n) @ acc)
        []
        next_nodes)
  in
  build_paths SS.empty start_node
;;

(* Problem 82 *)
let cycles graph node =
  let next_nodes = neighbors node graph.edges in
  let p = List.concat_map (fun n -> paths graph n node) next_nodes in
  List.map (fun p -> [ node ] @ p) p
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
  let _ =
    assert (
      length_sort
        [ [ "a"; "b"; "c" ]
        ; [ "d"; "e" ]
        ; [ "f"; "g"; "h" ]
        ; [ "d"; "e" ]
        ; [ "i"; "j"; "k"; "l" ]
        ; [ "m"; "n" ]
        ; [ "o" ]
        ]
      = [ [ "o" ]
        ; [ "d"; "e" ]
        ; [ "d"; "e" ]
        ; [ "m"; "n" ]
        ; [ "a"; "b"; "c" ]
        ; [ "f"; "g"; "h" ]
        ; [ "i"; "j"; "k"; "l" ]
        ])
  in
  let _ =
    assert (
      frequency_sort
        [ [ "a"; "b"; "c" ]
        ; [ "d"; "e" ]
        ; [ "f"; "g"; "h" ]
        ; [ "d"; "e" ]
        ; [ "i"; "j"; "k"; "l" ]
        ; [ "m"; "n" ]
        ; [ "o" ]
        ]
      = [ [ "i"; "j"; "k"; "l" ]
        ; [ "o" ]
        ; [ "a"; "b"; "c" ]
        ; [ "f"; "g"; "h" ]
        ; [ "d"; "e" ]
        ; [ "d"; "e" ]
        ; [ "m"; "n" ]
        ])
  in
  let _ = assert ((not (is_prime 1)) = true) in
  let _ = assert (is_prime 7 = true) in
  let _ = assert ((not (is_prime 12)) = true) in
  let _ = assert ((not (sieve 1)) = true) in
  let _ = assert (sieve 7 = true) in
  let _ = assert ((not (sieve 12)) = true) in
  let _ = assert (gcd 13 27 = 1) in
  let _ = assert (gcd 20536 7826 = 2) in
  let _ = assert (coprime 13 27 = true) in
  let _ = assert ((not (coprime 20536 7826)) = true) in
  let _ = assert (phi 10 = 4) in
  let _ = assert (phi 11 = 10) in
  let _ = assert (phi 1 = 1) in
  let _ = assert (phi 12 = 4) in
  let _ = assert (factors 315 = [ 3; 3; 5; 7 ]) in
  let _ = assert (factors 12 = [ 2; 2; 3 ]) in
  let _ = assert (factors_enc 315 = [ 3, 2; 5, 1; 7, 1 ]) in
  let _ = assert (phi_improved 10 = 4) in
  let _ = assert (phi_improved 13 = 12) in
  let _ = assert (timeit phi 10090 > timeit phi_improved 10090) in
  let _ = assert (List.length (all_primes 2 7920) == 1000) in
  let _ = assert (goldbach 28 = (5, 23)) in
  let _ =
    assert (
      goldbach_list 9 20
      = [ 10, (3, 7); 12, (5, 7); 14, (3, 11); 16, (3, 13); 18, (5, 13); 20, (3, 17) ])
  in
  let _ =
    assert (
      table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
      = [ true, true, true; true, false, true; false, true, false; false, false, false ])
  in
  let _ =
    assert (
      table2 "a" "b" (Not (And (Var "a", Var "b")))
      = [ true, true, false; true, false, true; false, true, true; false, false, true ])
  in
  let _ =
    assert (
      table [ "a"; "b" ] (And (Var "a", Or (Var "a", Var "b")))
      = [ [ "a", true; "b", true ], true
        ; [ "a", true; "b", false ], true
        ; [ "a", false; "b", true ], false
        ; [ "a", false; "b", false ], false
        ])
  in
  let _ = assert (gray 1 = [ "0"; "1" ]) in
  let _ = assert (gray 2 = [ "00"; "01"; "11"; "10" ]) in
  let _ = assert (gray 3 = [ "000"; "001"; "011"; "010"; "110"; "111"; "101"; "100" ]) in
  let _ =
    assert (
      huffman [ "a", 45; "b", 13; "c", 12; "d", 16; "e", 9; "f", 5 ]
      = [ "a", "0"; "c", "100"; "b", "101"; "f", "1100"; "e", "1101"; "d", "111" ])
  in
  let _ =
    assert (
      cbal_tree 4
      = [ Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty)))
        ; Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty))
        ; Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
        ; Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty))
        ])
  in
  let _ =
    assert (
      is_symmetric
        (Node
           ( 'x'
           , Node ('a', Node ('b', Empty, Empty), Node ('c', Empty, Empty))
           , Node ('a', Node ('c', Empty, Empty), Node ('b', Empty, Empty)) ))
      = true)
  in
  let _ = assert (is_symmetric (Node ('a', Node ('b', Empty, Empty), Empty)) = false) in
  let _ =
    assert (
      construct [ 3; 2; 5; 7; 1 ]
      = Node
          ( 3
          , Node (2, Node (1, Empty, Empty), Empty)
          , Node (5, Empty, Node (7, Empty, Empty)) ))
  in
  let _ = assert (is_symmetric (construct [ 5; 3; 18; 1; 4; 12; 21 ]) = true) in
  let _ = assert ((not (is_symmetric (construct [ 3; 2; 5; 7; 4 ]))) = true) in
  let _ =
    assert (
      sym_cbal_trees 5
      = [ Node
            ( 'x'
            , Node ('x', Empty, Node ('x', Empty, Empty))
            , Node ('x', Node ('x', Empty, Empty), Empty) )
        ; Node
            ( 'x'
            , Node ('x', Node ('x', Empty, Empty), Empty)
            , Node ('x', Empty, Node ('x', Empty, Empty)) )
        ])
  in
  let _ = assert (List.length (sym_cbal_trees 57) = 256) in
  let _ =
    assert (
      hbal_tree 3
      = [ Node
            ( 'x'
            , Node ('x', Empty, Node ('x', Empty, Empty))
            , Node ('x', Empty, Node ('x', Empty, Empty)) )
        ; Node
            ( 'x'
            , Node ('x', Empty, Node ('x', Empty, Empty))
            , Node ('x', Node ('x', Empty, Empty), Empty) )
        ; Node
            ( 'x'
            , Node ('x', Empty, Node ('x', Empty, Empty))
            , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) )
        ; Node
            ( 'x'
            , Node ('x', Node ('x', Empty, Empty), Empty)
            , Node ('x', Empty, Node ('x', Empty, Empty)) )
        ; Node
            ( 'x'
            , Node ('x', Node ('x', Empty, Empty), Empty)
            , Node ('x', Node ('x', Empty, Empty), Empty) )
        ; Node
            ( 'x'
            , Node ('x', Node ('x', Empty, Empty), Empty)
            , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) )
        ; Node
            ( 'x'
            , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
            , Node ('x', Empty, Node ('x', Empty, Empty)) )
        ; Node
            ( 'x'
            , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
            , Node ('x', Node ('x', Empty, Empty), Empty) )
        ; Node
            ( 'x'
            , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
            , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) )
        ; Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
        ; Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty))
        ; Node
            ( 'x'
            , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
            , Node ('x', Empty, Empty) )
        ; Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty)))
        ; Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty))
        ; Node
            ( 'x'
            , Node ('x', Empty, Empty)
            , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) )
        ])
  in
  let _ = assert (min_nodes 5 = 12) in
  let _ = assert (min_height 12 = 4) in
  let _ = assert (count_leaves Empty = 0) in
  let _ =
    assert (
      count_leaves
        (Node
           ( 'x'
           , Node ('x', Empty, Empty)
           , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) ))
      = 3)
  in
  let _ =
    assert (
      leaves
        (Node
           ( 'a'
           , Node ('b', Empty, Empty)
           , Node ('c', Node ('d', Empty, Empty), Node ('e', Empty, Empty)) ))
      = [ 'b'; 'd'; 'e' ])
  in
  let _ = assert (internals (Node ('a', Empty, Empty)) = []) in
  let _ =
    assert (
      internals
        (Node
           ( 'a'
           , Node ('b', Empty, Empty)
           , Node ('c', Node ('d', Empty, Empty), Node ('e', Empty, Empty)) ))
      = [ 'a'; 'c' ])
  in
  let _ =
    assert (
      at_level
        (Node
           ( 'a'
           , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
           , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) ))
        2
      = [ 'b'; 'c' ])
  in
  let _ =
    assert (
      complete_binary_tree [ 1; 2; 3; 4; 5; 6 ]
      = Node
          ( 1
          , Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty))
          , Node (3, Node (6, Empty, Empty), Empty) ))
  in
  let _ =
    assert (
      layout_binary_tree_1
        (Node
           ( 'n'
           , Node
               ( 'k'
               , Node
                   ( 'c'
                   , Node ('a', Empty, Empty)
                   , Node ('h', Node ('g', Node ('e', Empty, Empty), Empty), Empty) )
               , Node ('m', Empty, Empty) )
           , Node
               ('u', Node ('p', Empty, Node ('s', Node ('q', Empty, Empty), Empty)), Empty)
           ))
      = Node
          ( ('n', 8, 1)
          , Node
              ( ('k', 6, 2)
              , Node
                  ( ('c', 2, 3)
                  , Node (('a', 1, 4), Empty, Empty)
                  , Node
                      ( ('h', 5, 4)
                      , Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty)
                      , Empty ) )
              , Node (('m', 7, 3), Empty, Empty) )
          , Node
              ( ('u', 12, 2)
              , Node
                  ( ('p', 9, 3)
                  , Empty
                  , Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty) )
              , Empty ) ))
  in
  let _ =
    assert (
      string_of_tree
        (Node
           ( 'a'
           , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
           , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) ))
      = "a(b(d,e),c(,f(g,)))")
  in
  let _ =
    assert (
      tree_of_string "a(b(d,e),c(,f(g,)))"
      = Node
          ( 'a'
          , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
          , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) ))
  in
  let _ = assert (preorder (Node (1, Node (2, Empty, Empty), Empty)) = [ 1; 2 ]) in
  let _ = assert (inorder (Node (1, Node (2, Empty, Empty), Empty)) = [ 2; 1 ]) in
  let _ =
    assert (
      dotstring_tree
        (Node
           ( 'a'
           , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
           , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) ))
      = "abd..e..c.fg...")
  in
  let _ =
    assert (
      tree_dotstring "abd..e..c.fg..."
      = Node
          ( 'a'
          , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
          , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) ))
  in
  let mt =
    T
      ( 'a'
      , [ T ('f', [ T ('g', []) ]); T ('c', []); T ('b', [ T ('d', []); T ('e', []) ]) ]
      )
  in
  let _ = assert (string_of_mult_tree mt = "afg^^c^bd^e^^^") in
  let _ = assert (mult_tree_of_string "afg^^c^bd^e^^^" = mt) in
  let _ = assert (count_nodes (T ('a', [ T ('f', []) ])) = 2) in
  let _ = assert (ipl mt = 9) in
  let _ = assert (bottom_up (T ('a', [ T ('b', []) ])) = [ 'b'; 'a' ]) in
  let _ = assert (bottom_up mt = [ 'g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a' ]) in
  let _ = assert (lispy (T ('a', [])) = "a") in
  let _ = assert (lispy (T ('a', [ T ('b', []) ])) = "(a b)") in
  let _ = assert (lispy mt = "(a (f g) c (b d e))") in
  let example_graph =
    { nodes = [ 'b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k' ]
    ; edges = [ 'h', 'g'; 'k', 'f'; 'f', 'b'; 'f', 'c'; 'c', 'b' ]
    }
  in
  let _ = assert (gt_to_hf example_graph = "h-g k-f f-b f-c c-b d") in
  let _ =
    assert (
      hf_to_gt "b-c f-c g-h d f-b k-f h-g"
      = { nodes = [ 'b'; 'c'; 'd'; 'f'; 'g'; 'h' ]
        ; edges = [ 'h', 'g'; 'k', 'f'; 'f', 'b'; 'g', 'h'; 'f', 'c'; 'b', 'c' ]
        })
  in
  let _ = assert (paths example_graph 'f' 'b' = [ [ 'f'; 'c'; 'b' ]; [ 'f'; 'b' ] ]) in
  let _ =
    assert (
      cycles example_graph 'f'
      = [ [ 'f'; 'k'; 'f' ]
        ; [ 'f'; 'b'; 'c'; 'f' ]
        ; [ 'f'; 'b'; 'f' ]
        ; [ 'f'; 'c'; 'b'; 'f' ]
        ; [ 'f'; 'c'; 'f' ]
        ])
  in
  ()
;;
