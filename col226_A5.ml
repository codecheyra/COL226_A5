type symbol = string
type arity = int
type signature = (symbol * arity) list

type tree = V of string | C of { node: symbol; children: tree list }

let rec check_sig (sig_: signature) : bool =
  let rec check_duplicate_symbols symbols = match symbols with
    | [] -> false
    | x :: xs -> List.mem x xs || check_duplicate_symbols xs
  in
  let rec check_non_negative_arity arities = match arities with
    | [] -> true
    | (_, arity) :: rest -> arity >= 0 && check_non_negative_arity rest
  in
  not (check_duplicate_symbols (List.map fst sig_)) && check_non_negative_arity sig_

let rec wftree (t: tree) (sig_: signature) : bool =
  let rec check_arity symbol = 
    try
      let expected_arity = List.assoc symbol sig_ in
      match t with
      | V _ -> expected_arity = 0
      | C {node; children} ->
          let actual_arity = List.length children in
          expected_arity = actual_arity && List.for_all (fun child -> wftree child sig_) children
    with Not_found -> false
  in
  match t with
  | V s -> List.mem_assoc s sig_
  | C {node; children} -> check_arity node

let rec ht (t: tree) : int =
  match t with
  | V _ -> 1
  | C {node; children} -> 1 + List.fold_left (fun acc child -> max acc (ht child)) 0 children

let rec size (t: tree) : int =
  match t with
  | V _ -> 1
  | C {node; children} -> 1 + List.fold_left (fun acc child -> acc + size child) 0 children

let rec vars (t: tree) : string list =
  match t with
  | V s -> [s]
  | C {node; children} -> List.concat (List.map vars children)

let rec mirror (t: tree) : tree =
  match t with
  | V s -> V s
  | C {node; children} -> C {node; children = List.map mirror (List.rev children)}

type substitution = (string * tree) list

let check_subst (subst: substitution) : bool =
  let vars = List.map fst subst in
  let unique_vars = List.sort_uniq compare vars in
  List.length vars = List.length unique_vars

let rec compose_subst (s1: substitution) (s2: substitution) : substitution =
  let rec subst_tree t =
    match t with
    | V v -> (try List.assoc v s2 with Not_found -> V v)
    | C {node; children} -> C {node; children = List.map subst_tree children}
  in
  let apply_subst subst (v, t) = (v, subst t) in
  let s2' = List.map (apply_subst subst_tree) s1 in
  List.filter (fun (v, _) -> not (List.mem_assoc v s1)) (s1 @ s2')

let rec subst (t: tree) (s: substitution) : tree =
  let rec apply_subst_to_node s node =
    match node with
    | V v -> (try List.assoc v s with Not_found -> V v)
    | C {node; children} -> C {node; children = List.map (fun child -> subst child s) children}
  in
  match t with
  | V v -> (try List.assoc v s with Not_found -> V v)
  | C {node; children} -> apply_subst_to_node s t

exception NOT_UNIFIABLE

let rec mgu (t1: tree) (t2: tree) : substitution =
  let rec mgu_helper t1 t2 s =
    let t1' = subst t1 s in
    let t2' = subst t2 s in
    match t1', t2' with
    | V v1, V v2 when v1 = v2 -> s
    | V v, t | t, V v ->
        if List.mem_assoc v s then raise NOT_UNIFIABLE
        else (v, subst t s) :: s
    | C {node = n1; children = c1}, C {node = n2; children = c2} ->
        if n1 = n2 then
          List.fold_left2 (fun s c1 c2 -> mgu_helper c1 c2 s) s c1 c2
        else raise NOT_UNIFIABLE
  in
  mgu_helper t1 t2 []



















































  let rec print_tree indent tree = 
    match tree with
    | V s -> print_endline (indent ^ s)
    | C {node; children} -> 
        print_endline (indent ^ node);
        List.iter (fun child -> print_tree (indent ^ "  ") child) children

let mgu1 = mgu (V "x") (V "y")
let () = print_endline "mgu1:"; List.iter (fun (v, t) -> Printf.printf "%s -> " v; print_tree "" t) mgu1

(* let mgu2 = mgu (C { node = "a"; children = [] }) (C { node = "a"; children = [] })
let () = print_endline "mgu2:"; List.iter (fun (v, t) -> Printf.printf "%s -> " v; print_tree "" t) mgu2 *)

let mgu3 = mgu (C { node = "a"; children = [] }) (C { node = "b"; children = [] })
let () = print_endline "mgu3:"; List.iter (fun (v, t) -> Printf.printf "%s -> " v; print_tree "" t) mgu3

(* let my_tree = C { node = "+"; children = [ C { node = "+"; children = [] }; C { node = "+"; children = [ V "2"; V "3"; ]}; ];};; *)

(* sir testcase 1 *)
(* let example_sig = [("0", 0); ("1", 0); ("0", 1)];;
let is_valid = check_sig example_sig;;
print_endline(string_of_bool is_valid);; *)

(* sir testcase 2 *)
(* let example_sig = [("0", 0); ("1", 0); ("+", 2)];;
let t = C {node = ("+"); children = [(V "+"); (V "+"); (V "+")]};;
let is_valid = wftree t example_sig;;
print_endline(string_of_bool is_valid);; *)

(* sir testcase 3 *)
(* let t2 = C {node = ("+"); children = [(V "x"); (V "y")]} ;;
let height = ht t2;;
print_endline(string_of_int height);; *)

(* sir testcase 4 *)
(* let t2 = C {node = ("+"); children = [(V "x"); (V "y")]} ;;
let size = size t2;;
print_endline(string_of_int size);; *)

(* sir testcase 5 *)
(* let t3 = C {node = "+"; children = [V "z"; t2]};;
print_endline "my tree is:";;
print_tree "" t3;;
let mirrored_tree = mirror t3;;
print_endline "my mirrored tree is:";;
print_tree "" mirrored_tree;; *)