open Funs

				(***********************)
				(* Part 2: Integer BTS *)
				(***********************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with
  | IntLeaf -> 0
  | IntNode(_,l,r) -> 1 + (int_size l) + (int_size r)

let rec int_max t = 
  match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode(v,_,r) -> if r = IntLeaf then v else int_max r  

let rec int_common t x y =
  if not(int_mem x t) || not(int_mem y t) then 
    raise (Invalid_argument("int_common"))
  else if x = y then x
  else match t with
       | IntLeaf -> raise (Invalid_argument("int_common"))
       | IntNode(v,_,_) when v = x || v = y -> v
       | IntNode(v,_,_) when v < x && v > y -> v
       | IntNode(v,_,_) when v > x && v < y -> v 
       | IntNode(v,l,r) -> if v > x && v > y then int_common l x y
                           else int_common r x y

				(***************************)
				(* Part 3: Polymorphic BTS *)
				(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree

type 'a compfn = 'a -> 'a -> int

type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

let pinsert x t =  
  match t with
  | (f,Leaf) -> (f,Node(x,Leaf,Leaf))
  | (f,Node(v,l,r)) ->   
    let rec helper x at =
      match at with
      | Leaf -> Node(x,Leaf,Leaf)
      | Node(v,l,r) when (f x v) < 0 -> Node(v, helper x l, r)
      | Node(v,l,r) when (f x v) > 0 -> Node(v, l, helper x r)
      | Node(v,l,r) -> at
    in (f, helper x (Node(v,l,r)))                                                                   
                                                                      
let pmem x t = 
  match t with
  | (f,Leaf) -> false
  | (f,Node(v,l,r)) ->
    let rec helper x at =
      match at with
      | Leaf -> false
      | Node(v,l,r) when (f x v) < 0 -> helper x l
      | Node(v,l,r) when (f x v) > 0 -> helper x r
      | Node(v,l,r) -> true 
    in helper x (Node(v,l,r))                                                                                                                            
                                                                     
let pinsert_all lst t = fold (fun acc x -> pinsert x acc) t lst

let rec p_as_list t = 
  match t with
  | (f,Leaf) -> []
  | (f,Node(v,l,r)) ->   
    let rec helper at acc =
      match at with
      | Leaf -> acc
      | Node(v,l,r) -> 
        let right_tree = helper r acc in
        let curr_tree = v :: right_tree in
        helper l curr_tree 
    in helper (Node(v,l,r)) []

let pmap fn t = match t with
  (f,_) -> pinsert_all (map fn (p_as_list t)) (empty_ptree f)

				(***************************)
				(* Part 4: Variable Lookup *)
				(***************************)

type lookup_table = 
  | Tail
  | Head
  | Scope of (string * int) list * lookup_table * lookup_table

let empty_table () : lookup_table = Tail

let push_scope (table: lookup_table) : lookup_table = 
  let rec helper table =
    match table with
    | Tail -> Scope([], Head, Tail) 
    | Scope(list, head, tail) when head = Head -> Scope(list, Scope([], Head, table), tail)
    | Scope(list, head, tail) -> helper head
  in helper table   

let pop_scope (table: lookup_table) : lookup_table = 
  let rec helper table =
    match table with
    | Tail -> raise (failwith("No scopes remain!"))
    | Scope(list, head, tail) when head = Head -> tail
    | Scope(list, head, tail) -> helper head
  in helper table

let add_var name value (table: lookup_table) : lookup_table =  
  let rec helper table =
    match table with
    | Tail -> raise (failwith("There are no scopes to add a variable to!"))
    | Scope(list, head, tail) when head = Head ->
      let rec helper list = 
        match list with
        | [] -> [(name, value)]
        | (hk,hv) :: t -> if hk = name then (name,value) :: t
                          else (hk,hv) :: helper t
      in Scope((helper list), head, tail)
    | Scope(list, head, tail) -> helper head
  in helper table 

let rec lookup name (table: lookup_table) = 
  match table with 
  | Tail -> raise (failwith("Variable not found!"))
  | Scope(list, head, tail) -> 
    let rec helper list =
      match list with
      | [] -> raise (failwith("Variable not found!"))
      | (hk,hv) :: t -> if hk = name then hv else helper t
    in helper list