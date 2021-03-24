open SmallCTypes
open EvalUtils
open TokenTypes
open List

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t = 
  match t with
  | Int x -> Int_Val x
  | Bool x -> Bool_Val x
  | ID x -> if (List.mem_assoc x env) = true then
              (List.assoc x env)
            else raise (DeclareError("DeclareError"))

  | Add (e1, e2) -> let x1 = (eval_expr env e1) in
                    let x2 = (eval_expr env e2) in
                    (match x1 with
                     | Int_Val i1 -> (match x2 with
                                      | Int_Val i2 -> (Int_Val(i1+i2))
                                      | Bool_Val b2 -> raise (TypeError("bool add")))
                     | Bool_Val b1 -> raise (TypeError("bool add")))

  | Sub (e1, e2) -> let x1 = (eval_expr env e1) in
                    let x2 = (eval_expr env e2) in
                    (match x1 with
                     | Int_Val i1 -> (match x2 with
                                      | Int_Val i2 -> (Int_Val(i1-i2))
                                      | Bool_Val b2 -> raise (TypeError("bool sub")))
                     | Bool_Val b1 -> raise (TypeError("bool sub")))

  | Mult (e1, e2) -> let x1 = (eval_expr env e1) in
                     let x2 = (eval_expr env e2) in
                     (match x1 with
                      | Int_Val i1 -> (match x2 with
                                       | Int_Val i2 -> (Int_Val(i1*i2))
                                       | Bool_Val b2 -> raise (TypeError("bool mult")))
                      | Bool_Val b1 -> raise (TypeError("bool mult"))) 

  | Div (e1, e2) -> let x1 = (eval_expr env e1) in
                    let x2 = (eval_expr env e2) in
                    (match x1 with
                     | Int_Val i1 -> (match x2 with
                                      | Int_Val i2 -> if i2 = 0 then
                                                      raise (DivByZeroError)
                                                      else (Int_Val(i1/i2))
                                      | Bool_Val b2 -> raise (TypeError("bool div")))
                     | Bool_Val b1 -> raise (TypeError("bool div")))
  
  | Pow (e1, e2) -> let x1 = (eval_expr env e1) in
                    let x2 = (eval_expr env e2) in
                    (match x1 with
                     | Int_Val i1 -> (match x2 with
                                      | Int_Val i2 -> let f1 = (float_of_int i1) in
                                                      let f2 = (float_of_int i2) in
                                                      Int_Val(int_of_float((f1)**(f2)))
                                      | Bool_Val b2 -> raise (TypeError("bool pow")))
                     | Bool_Val b1 -> raise (TypeError("bool pow")))

  | Or (e1, e2) -> let x1 = (eval_expr env e1) in
                   let x2 = (eval_expr env e2) in
                   (match x1 with
                    | Bool_Val b1 -> (match x2 with
                                      | Bool_Val b2 -> (Bool_Val(b1||b2))
                                      | Int_Val i2 -> raise (TypeError("int or")))
                    | Int_Val i1 -> raise (TypeError("int or")))

  | And (e1, e2) -> let x1 = (eval_expr env e1) in 
                    let x2 = (eval_expr env e2) in
                    (match x1 with
                     | Bool_Val b1 -> (match x2 with
                                       | Bool_Val b2 -> (Bool_Val(b1&&b2))
                                       | Int_Val i2 -> raise (TypeError("int or")))
                     | Int_Val i1 -> raise (TypeError("int or")))

  | Not e -> let x = (eval_expr env e) in
             (match x with
              | Bool_Val b1 -> (Bool_Val(not b1))
              | Int_Val i1 -> raise (TypeError("int not")))

  | Greater (e1, e2) -> let x1 = (eval_expr env e1) in
                        let x2 = (eval_expr env e2) in
                        (match x1 with
                         | Int_Val i1 -> (match x2 with
                                          | Int_Val i2 -> (Bool_Val(i1>i2))
                                          | Bool_Val b2 -> raise (TypeError("bool >")))
                         | Bool_Val b1 -> raise (TypeError("bool >")))

  | Less (e1, e2) -> let x1 = (eval_expr env e1) in
                     let x2 = (eval_expr env e2) in 
                     (match x1 with
                      | Int_Val i1 -> (match x2 with
                                       | Int_Val i2 -> (Bool_Val(i1<i2))
                                       | Bool_Val b2 -> raise (TypeError("bool <")))
                      | Bool_Val b1 -> raise (TypeError("bool <")))

  | GreaterEqual (e1, e2) -> let x1 = (eval_expr env e1) in
                             let x2 = (eval_expr env e2) in
                             (match x1 with
                              | Int_Val i1 -> (match x2 with
                                               | Int_Val i2 -> (Bool_Val(i1>=i2))
                                               | Bool_Val b2 -> raise (TypeError("bool >=")))
                              | Bool_Val b1 -> raise (TypeError("bool >=")))

  | LessEqual (e1, e2) -> let x1 = (eval_expr env e1) in
                          let x2 = (eval_expr env e2) in
                          (match x1 with 
                           | Int_Val i1 -> (match x2 with
                                            | Int_Val i2 -> (Bool_Val(i1<=i2))
                                            | Bool_Val b2 -> raise (TypeError("bool <=")))
                           | Bool_Val b1 -> raise (TypeError("bool >=")))

  | Equal (e1, e2) -> let x1 = (eval_expr env e1) in
                      let x2 = (eval_expr env e2) in
                      (match x1 with
                       | Int_Val i1 -> (match x2 with
                                        | Int_Val i2 -> (Bool_Val(i1==i2))
                                        | Bool_Val b2 -> raise (TypeError("bool ==")))
                       | Bool_Val b1 -> (match x2 with
                                         | Int_Val i2 -> raise (TypeError("int =="))
                                         | Bool_Val b2 -> (Bool_Val(b1==b2))))
                                          
  | NotEqual (e1, e2) -> let x1 = (eval_expr env e1) in 
                         let x2 = (eval_expr env e2) in
                         (match x1 with
                          | Int_Val i1 -> (match x2 with
                                           | Int_Val i2 -> (Bool_Val(i1!=i2))
                                           | Bool_Val b2 -> raise (TypeError("bool !=")))
                          | Bool_Val b1 -> (match x2 with
                                            | Int_Val i2 -> raise (TypeError("int !="))
                                            | Bool_Val b2 -> (Bool_Val(b1!=b2))))

let rec eval_stmt env s =
  match s with
  | NoOp -> env
  | Seq (stmt1, stmt2) -> (eval_stmt (eval_stmt env stmt1) stmt2)  
  | Declare (t, id) -> if (List.mem_assoc id env) = true then
                         raise (DeclareError("eval_stmt declar id error"))
                       else (match t with 
                             | Int_Type -> env @ [(id, (Int_Val(0)))]
                             | Bool_Type -> env @ [(id, (Bool_Val(false)))])

  | Assign (id, v) -> if (mem_assoc id env) = false then
                        raise (DeclareError("eval_stmt assign id error"))
                      else 
                        let id_type = (assoc id env) in
                        let value = (eval_expr env v) in
                        (match id_type with
                         | Int_Val i -> 
                             (match value with
                              | Int_Val x -> let env2 = remove_assoc id env in
                                             env2 @ [(id, (Int_Val(x)))]
                              | Bool_Val _ -> raise (TypeError("eval_stmt assign bool fail")))
                         | Bool_Val b -> 
                             (match value with
                              | Bool_Val x -> let env2 = remove_assoc id env in
                                              env2 @ [(id, (Bool_Val(x)))]
                              | Int_Val _ -> raise (TypeError("eval_stmt assign int fail"))))

  | If (expr, stmt1, stmt2) -> let e1 = (eval_expr env expr) in
                     (match e1 with
                     | Int_Val i -> raise (TypeError("eval_stmt if int"))
                     | Bool_Val b1 -> match b1 with 
                                      | true -> (eval_stmt env stmt1)
                                      | _ -> (eval_stmt env stmt2))
  
  | While (expr, stmt) -> let e1 = (eval_expr env expr) in
                          (match e1 with
                           | Int_Val i -> raise (TypeError("eval_stmt while int"))
                           | Bool_Val b1 -> 
                               (match b1 with
                                | true -> (eval_stmt (eval_stmt env stmt) (While(expr,stmt)))
                                | _ -> env))
 
(*  | For (id, expr1, expr2, stmt) -> let i1 = (eval_expr env expr1) in*)
(*                                    let i2 = (eval_expr env expr2) in*)
(*                                    (match i1 with*)
  
  | Print (expr) -> let e1 = (eval_expr env expr) in
                    (match e1 with 
                     | Int_Val i -> (print_output_int i);
                                    (print_output_newline());
                                     env
                     | Bool_Val b -> (print_output_bool b);
                                     (print_output_newline());
                                     env)