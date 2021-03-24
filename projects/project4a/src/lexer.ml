open TokenTypes
open Str

let re = [
  ("(", fun _ -> [Tok_LParen]);
  (")", fun _ -> [Tok_RParen]);
  ("{", fun _ -> [Tok_LBrace]);
  ("}", fun _ -> [Tok_RBrace]);
  ("==", fun _ -> [Tok_Equal]);
  ("=", fun _ -> [Tok_Assign]);
  ("!=", fun _ -> [Tok_NotEqual]);
  (">=", fun _ -> [Tok_GreaterEqual]);
  ("<=", fun _ -> [Tok_LessEqual]);
  (">", fun _ -> [Tok_Greater]);
  ("<", fun _ -> [Tok_Less]);
  ("||", fun _ -> [Tok_Or]);
  ("&&", fun _ -> [Tok_And]);
  ("!", fun _ -> [Tok_Not]);
  (";", fun _ -> [Tok_Semi]);
  ("+", fun _ -> [Tok_Add]);
  ("*", fun _ -> [Tok_Mult]);
  ("/", fun _ -> [Tok_Div]);
  ("\\^", fun _ -> [Tok_Pow]);
  ("int", fun _ -> [Tok_Int_Type]);
  ("bool", fun _ -> [Tok_Bool_Type]);
  ("printf", fun _ -> [Tok_Print]);
  ("main", fun _ -> [Tok_Main]);
  ("if", fun _ -> [Tok_If]);
  ("else", fun _ -> [Tok_Else]);
  ("for", fun _ -> [Tok_For]);
  ("from", fun _ -> [Tok_From]);
  ("to", fun _ -> [Tok_To]);
  ("while", fun _ -> [Tok_While]);
  ("-?[0-9]+", fun x -> [Tok_Int (int_of_string x)]);
  ("true\\|false", fun x -> [Tok_Bool (bool_of_string x)]);
  ("[a-zA-Z][a-zA-Z0-9]*", fun x -> [Tok_ID x]);
  ("-", fun _ -> [Tok_Sub]);  
  ("[ \t\n]*", fun _ -> [])]

let tokenize input =
  let rec helper str pos = 
    if pos >= (String.length str) then 
      [EOF]
    else
      let (_, f) = List.find
          (fun (r,_) -> string_match (regexp r) str pos) re in
      let m_s = matched_string str in
      if (m_s = "int" || m_s = "if" || m_s = "bool" || m_s = "printf" 
         || m_s = "main" || m_s = "for" || m_s = "from" || m_s = "to" 
         || m_s = "while" || m_s = "true" || m_s = "false" || m_s = "else") then
        let new_pos = (pos + (String.length m_s)) in 
        if (string_match (regexp "[a-zA-Z0-9]+") str new_pos) then
          let m_s2 = matched_string str in
          (Tok_ID (m_s^m_s2)) :: (helper str (pos + (String.length m_s) + (String.length m_s2)))
        else 
          (f m_s) @ (helper str (new_pos))
      else 
        (f m_s) @ (helper str (pos + (String.length m_s)))
in helper input 0
