open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  parse_or toks 

and parse_or lst =
  let (lst2, p1) = (parse_and lst) in
  match lst2 with
  | Tok_Or :: t -> let (lst3, p2) = (parse_or t) in
                   (lst3, Or(p1,p2))
  | _ -> (lst2, p1)

and parse_and lst =
  let (lst2, p1) = (parse_equality lst) in
  match lst2 with
  | Tok_And :: t -> let (lst3, p2) = (parse_and t) in
                    (lst3, And(p1, p2))
  | _ -> (lst2, p1)

and parse_equality lst =
  let (lst2, p1) = (parse_relational lst) in
  match lst2 with
  | Tok_Equal :: t -> let (lst3, p2) = (parse_equality t) in
                      (lst3, Equal(p1, p2))
  | Tok_NotEqual :: t -> let (lst3, p2) = (parse_equality t) in
                         (lst3, NotEqual(p1, p2))
  | _ -> (lst2, p1)

and parse_relational lst =
  let (lst2, p1) = (parse_additive lst) in
  match lst2 with
  | Tok_Less :: t -> let (lst4, p2) = (parse_relational t) in
                     (lst4, Less(p1,p2))
  | Tok_Greater :: t -> let (lst4, p2) = (parse_relational t) in
                        (lst4, Greater(p1,p2))
  | Tok_LessEqual :: t -> let (lst4, p2) = (parse_relational t) in
                          (lst4, LessEqual(p1,p2))
  | Tok_GreaterEqual :: t -> let (lst4, p2) = (parse_relational t) in
                             (lst4, GreaterEqual(p1,p2))
  | _ -> (lst2, p1)

and parse_additive lst =
  let (lst2, p1) = (parse_multiplicative lst) in
  match lst2 with
  | Tok_Add :: t -> let (lst4, p2) = (parse_additive t) in 
                    (lst4, Add(p1,p2))
  | Tok_Sub :: t -> let (lst4, p2) = (parse_additive t) in 
                    (lst4, Sub(p1,p2))
  | _ -> (lst2, p1)

and parse_multiplicative lst =
  let (lst2, p1) = (parse_power lst) in
  match lst2 with 
  | Tok_Mult :: t -> let (lst4, p2) = (parse_multiplicative t) in
                     (lst4, Mult(p1,p2))
  | Tok_Div :: t -> let (lst4, p2) = (parse_multiplicative t) in
                    (lst4, Div(p1,p2))
  | _ -> (lst2, p1)

and parse_power lst =
  let (lst2, p1) = (parse_unary lst) in
  match lst2 with 
  | Tok_Pow :: t -> let (lst4, p2) = (parse_power t) in 
                   (lst4, Pow(p1,p2))
  | _ -> (lst2, p1)

and parse_unary lst =
  match lst with
  | Tok_Not :: t -> let (lst2, p) = (parse_unary t) in
                    (lst2, Not(p))
  | _ -> parse_primary lst

and parse_primary lst =
  match lst with
  | Tok_Int(v) :: t -> (t, Int(v))
  | Tok_Bool(v) :: t -> (t, Bool(v))
  | Tok_ID(v) :: t -> (t, ID (v))
  | Tok_LParen :: t -> (let (lst3, p) = (parse_expr t) in
                       match lst3 with
                       | Tok_RParen :: t -> (t, p)
                       | _ -> raise (InvalidInputException("Right paren errror")))
  | _ -> raise (InvalidInputException("parse_exprs error"))

let rec parse_stmt toks : stmt_result =
  match toks with
  | Tok_Int_Type :: t -> let (lst, int) = (parse_declare toks) in
                         if lookahead lst = Tok_RBrace then
                           (lst, Seq(int, NoOp))
                         else
                           let (lst2, stmt) = (parse_stmt lst) in
                           (lst2, Seq(int, stmt))

  | Tok_Bool_Type :: t -> let (lst, bool) = (parse_declare toks) in
                          if lookahead lst = Tok_RBrace then
                            (lst, Seq(bool, NoOp)) 
                          else
		            let (lst2, stmt) = (parse_stmt lst) in
		            (lst2, Seq(bool, stmt))

  | Tok_ID(x) :: t -> let (lst, assign) = (parse_assign toks) in
                      if lookahead lst = Tok_RBrace then
                        (lst, Seq(assign, NoOp)) 
                      else
                        let (lst2, stmt) = (parse_stmt lst) in
                        (lst2, Seq(assign, stmt))

  | Tok_Print :: t -> let (lst, print) = (parse_print toks) in
                      if lookahead lst = Tok_RBrace then
                        (lst, Seq(print, NoOp))
                      else
                        let (lst2, stmt) = (parse_stmt lst) in
                        (lst2, Seq(print, stmt))

  | Tok_If :: t -> let (lst, if_stmt) = (parse_if toks) in
                   if lookahead lst = Tok_RBrace then
                     (lst, Seq(if_stmt, NoOp))
                   else
                     let (lst2, stmt) = (parse_stmt lst) in
                     (lst2, Seq(if_stmt, stmt))

  | Tok_For :: t -> let (lst, for_stmt) = (parse_for toks) in
                    if lookahead lst = Tok_RBrace then
                      (lst, Seq(for_stmt, NoOp))
                    else
                      let (lst2, stmt) = (parse_stmt lst) in
                      (lst2, Seq(for_stmt, stmt))

  | Tok_While :: t -> let (lst, while_stmt) = (parse_while toks) in
                      if lookahead lst = Tok_RBrace then
                        (lst, Seq(while_stmt, NoOp))
                      else
                        let (lst2, stmt) = (parse_stmt lst) in
                        (lst2, Seq(while_stmt, stmt))

  | EOF :: t -> (toks, NoOp)
    
  | _ -> raise (InvalidInputException("not a statement error")) 

and parse_declare toks = 
  match toks with
  | Tok_Int_Type :: t -> (match t with 
                         | Tok_ID(v) :: t2 -> let lst = (match_token t2 Tok_Semi) in
                                              (lst, Declare(Int_Type, v))
                         | _ -> raise (InvalidInputException("parse_declare int error")))

  | Tok_Bool_Type :: t -> (match t with
                         | Tok_ID(v) :: t2 -> let lst = (match_token t2 Tok_Semi) in
                                              (lst, Declare(Bool_Type, v))
                         | _ -> raise (InvalidInputException("parse_declare bool error")))
  | _ -> raise (InvalidInputException("parse_declare error"))

and parse_assign toks =
  match toks with
  | Tok_ID(v) :: t -> let lst = (match_token t Tok_Assign) in
                      let (lst2, p1) = (parse_expr lst ) in
                      let lst3 = (match_token lst2 Tok_Semi) in
                      (lst3, Assign(v, p1))
  | _ -> raise (InvalidInputException("parse_assign error"))

and parse_if toks =
  match toks with
  | Tok_If :: t -> let lst = (match_token t Tok_LParen) in
                   let (lst1, if_expr) = (parse_expr lst) in
                   let lst2 = (match_token lst1 Tok_RParen) in
                   let lst3 = (match_token lst2 Tok_LBrace) in
                   let (lst4, if_stmt) = (parse_stmt lst3) in
                   let lst5 = (match_token lst4 Tok_RBrace) in
                   (match lst5 with
                    | Tok_Else :: t2 -> let lst6 = (match_token t2 Tok_LBrace) in
                                        let (lst7, else_stmt) = (parse_stmt lst6) in
                                        let lst8 = (match_token lst7 Tok_RBrace) in
                                        (lst8, If(if_expr, if_stmt, else_stmt))
                    | _ -> (lst5, If(if_expr, if_stmt, NoOp)))
  | _ -> raise (InvalidInputException("parse_if error"))

and parse_print toks = 
  match toks with
  | Tok_Print :: t -> let lst = (match_token t Tok_LParen) in
                      let (lst2, expr) = (parse_expr lst) in
                      let lst3 = (match_token lst2 Tok_RParen) in 
                      let lst4 = (match_token lst3 Tok_Semi) in
                      (lst4, Print(expr))
  | _ -> raise (InvalidInputException("parse_print error"))

and parse_for toks =
  match toks with
  | Tok_For :: t -> let lst = (match_token t Tok_LParen) in
                    (match lst with
                     | Tok_ID(v) :: t2 -> let lst2 = (match_token t2 Tok_From) in
                                          let (lst3, from_expr) = (parse_expr lst2) in
                                          let lst4 = (match_token lst3 Tok_To) in
                                          let (lst5, to_expr) = (parse_expr lst4) in
                                          let lst6 = (match_token lst5 Tok_RParen) in
                                          let lst7 = (match_token lst6 Tok_LBrace) in
                                          let (lst8, stmt) = (parse_stmt lst7) in
                                          let lst9 = (match_token lst8 Tok_RBrace) in
                                          (lst9, For(v, from_expr, to_expr, stmt))
                     | _ -> raise (InvalidInputException("parse_for ID error")))
  | _ -> raise (InvalidInputException("parse_for error"))

and parse_while toks =
  match toks with 
  | Tok_While :: t -> let lst = (match_token t Tok_LParen) in
                      let (lst2, expr) = (parse_expr lst) in
                      let lst3 = (match_token lst2 Tok_RParen) in
                      let lst4 = (match_token lst3 Tok_LBrace) in
                      let (lst5, stmt) = (parse_stmt lst4) in
                      let lst6 = (match_token lst5 Tok_RBrace) in
                      (lst6, While(expr, stmt))
  | _ -> raise (InvalidInputException("parse_while error"))

let parse_main toks : stmt =
  let lst = (match_token toks Tok_Int_Type) in  
  let lst1 = (match_token lst Tok_Main) in
  let lst2 = (match_token lst1 Tok_LParen) in
  let lst3 = (match_token lst2 Tok_RParen) in
  let lst4 = (match_token lst3 Tok_LBrace) in
  if lookahead lst4 = Tok_RBrace then
    NoOp
  else
    let (lst5, stmt) = (parse_stmt lst4) in
    let lst6 = (match_token lst5 Tok_RBrace) in
    if lookahead lst6 <> EOF then
      raise (InvalidInputException("EOF not at the end"))
    else      
      stmt