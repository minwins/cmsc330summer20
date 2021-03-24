open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = 
  fold (fun counter element -> if target = element then counter+1
                               else counter) 0 lst;;

let uniq lst = 
  fold (fun acc t -> if count_occ acc t = 0 then t::acc
                       else acc) [] lst;; 

let assoc_list lst = 
 uniq (fold (fun acc t -> (t, count_occ lst t)::acc) [] lst);;
