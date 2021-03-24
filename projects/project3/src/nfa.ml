open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let count_occ lst target = 
  fold_left (fun counter element -> if target = element then counter+1
                               else counter) 0 lst;;

let uniq lst = 
  fold_left (fun acc t -> if count_occ acc t = 0 then t::acc
                       else acc) [] lst;;

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =   
  let rec helper lst acc =
    match lst with 
    | [] -> acc
    | h :: t -> 
      let rec helper1 tranList acc = 
        match tranList with
        | [] -> acc
        | (p1, v, p2) :: t when (h = p1) && (v = s) -> 
          let acc = p2 :: acc in helper1 t acc 
        | (p1, v, p2) :: t -> helper1 t acc
      in let acc = helper1 nfa.delta acc in
      helper t acc
  in uniq (helper qs [])
;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec helper lst acc =
    let lst = uniq(acc @ (move nfa acc (None))) in
    if not(List.compare_lengths lst acc = 0) then helper lst lst else lst
  in helper qs qs
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
  let str = explode s in
  
  let check = List.fold_left (fun acc t -> if not(List.mem t nfa.sigma) then
     false else acc) true str in
  
  if (check = false) then false else

  let rec helper nfa lst acc =
    match lst with
    | [] -> acc
    | h :: t -> let acc = (e_closure nfa (move nfa acc (Some h))) in (helper nfa t acc)
  in let final = helper nfa str (e_closure nfa [nfa.q0]) in
  
  let rec helper lst acc =
    match lst with 
    | [] -> acc
    | h :: t when List.mem h final -> let acc = true in helper t acc
    | h :: t -> helper t acc
  in helper nfa.fs false
;;

(****************)
(* Part 2: DFAs *)
(****************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  uniq(List.fold_left (fun acc t ->
   (e_closure nfa (move nfa qs (Some t))) :: acc) [] nfa.sigma)
;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  uniq(List.fold_left (fun acc t -> 
    (qs, (Some t), (e_closure nfa (move nfa qs (Some t)))) :: acc) [] nfa.sigma)
;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  uniq(List.fold_left (fun acc t -> if List.mem t qs then 
    qs :: acc else acc) [] nfa.fs)
;;

let find_new_qs (nfa: ('q,'s) nfa_t) : 'q list list =
  let first_states = (new_states nfa nfa.qs) in  
  let states = e_closure nfa [nfa.q0] :: first_states in
  let new_qs = List.fold_left ( fun acc t -> List.fold_left (fun acc t ->
    if t = [] then acc else t :: acc) acc (new_states nfa t)) [] states in
  uniq(List.fold_left (fun acc t -> t :: acc) states new_qs)
;;

let find_new_trans (nfa: ('q,'s) nfa_t) : ('q list, 's) transition list =
  let new_qs = find_new_qs nfa in
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | h :: t -> let ace = new_trans nfa h in
      let acc = List.fold_left (fun acc t -> t :: acc) acc ace in
      helper t acc
  in uniq(helper new_qs [])
;;

let find_new_finals (nfa: ('q,'s) nfa_t) : 'q list list =
  let new_qs = find_new_qs nfa in
  let rec helper lst acc = 
    match lst with
    | [] -> acc
    | h :: t -> let ace = new_finals nfa h in
      let acc = List.fold_left (fun acc t -> t :: acc) acc ace in
      helper t acc
   in uniq(helper new_qs [])
;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  { qs= (find_new_qs nfa)
  ; sigma= (nfa.sigma)
  ; delta= (find_new_trans nfa)
  ; q0= (e_closure nfa [nfa.q0])
  ; fs= (find_new_finals nfa) }
;;