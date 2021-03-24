(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (a, b, c) =
  (c, b, a);;

let abs x = 
if x >= 0 then x else -x;;

let area (x1, y1) (x2, y2) =
  abs(x1 - x2) * abs(y1 - y2);;

let volume (x1, y1, z1) (x2, y2, z2) =
  abs(x1 - x2) * abs(y1 - y2) * abs(z1 - z2);; 

let equiv_frac (a, b) (x, y) = 
if b != 0 || y != 0 then
  if (float (a) /. float (b)) = (float (x) /. float (y)) then
    true
  else
    false
else
  false;;  

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x = 
  if x = 0 then
    1
  else 
    x * factorial (x-1);;

let rec pow x p = (*p is non negative, but can be 0*) 
  if p = 0 then
    1
  else 
    x * (pow x (p - 1));;    

let rec len x = 
  let n = abs(x) in
  if n < 10 then
    1
  else  
    1 + len(n/10);; 

let rec tail x i =  
  if i = 0 then
    0
  else
    if i >= len(x) then
      x
    else
     x mod pow 10 i;;  

let rec contains sub x = 
  if len(sub) > len(x) then
    false
  else
    let i = len(sub) in
    let k = len(x) in
    if (x / pow 10 (k-i)) = sub then
      true
    else
      if k-1 = 0 then
        false
      else
        contains sub (tail x (k-1));; 

let rec length_ps list = 
  match list with
  | [] -> 0
  | (_::t) -> 1 + length_ps t;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst =
  let len = idx + 1 in
  if len > length_ps lst then 
    raise (failwith "Out of bounds")
  else 
    match lst with
    | [] -> raise (failwith "Out of bounds")
    | h :: t -> if idx = 0 then 
                  h
                else get (idx-1) t;;

let rec combine list1 list2 = 
  match list1 with
  | [] -> list2
  | h :: t -> h :: combine t list2;;                          

let rec reverse lst =  
  match lst with
  | [] -> []
  | h :: t -> combine (reverse t) [h];; 

let split lst =
  match lst with
  | [] -> []
  | _ :: t -> t;;

let rotate_once lst = 
  let new_head = get 0 lst in
  let new_tail = split lst in 
  combine new_tail [new_head];;

let rec rotate shift lst =  
  if shift = 0 then
    lst
  else
    rotate (shift-1) (rotate_once (lst));;