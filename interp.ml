(* Maciej Kowalczyk | Liam Concannon *)
(* I pledge my Honor that I have abided by the Stevens Honor System *)
open Ast
open Ds


let rec apply_proc : exp_val -> exp_val -> exp_val ea_result =
  fun f a ->
  match f with
  | ProcVal (id,body,env) ->
    return env >>+
    extend_env id a >>+
    eval_expr body
  | _ -> error "apply_proc: Not a procVal"
and
 eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2
  | Abs(e1)      ->
    eval_expr e1  >>=
    int_of_numVal >>= fun n ->
    return @@ NumVal (abs n)

  | Cons(e1, e2) ->
    eval_expr e1 >>= fun lst1 ->
    eval_expr e2 >>=
    list_of_listVal >>= fun lst2 ->
    return @@ ListVal(lst1::lst2)     
 
 | Hd(e1) ->
   eval_expr e1 >>= 
   list_of_listVal >>= fun lst ->
   if List.length lst = 0
   then failwith "Error: List cannot be empty"    
   else return @@ (List.hd lst)
  
  | Tl(e1) ->  
    eval_expr e1 >>=
    list_of_listVal >>= fun lst ->
    if List.length lst = 0
    then failwith "Error: List cannot be emtpy"
    else return @@ ListVal(List.tl lst)

  | Empty(e1)  ->
    eval_expr e1 >>=
    list_of_listVal >>= fun lst ->
    if List.length lst = 0 
    then return (BoolVal true)
    else return (BoolVal false)

  | EmptyList  -> return @@ ListVal([])

  | Tuple(es) -> 
    sequence (List.map eval_expr es) >>= fun tup ->
    return (TupleVal(tup))

  | Untuple(ids,e1,e2) ->  
    eval_expr e1 >>= fun tup ->
    if (is_tupleVal tup == false)
    then error "Expected a tuple!"
    else
    if (List.length ids != List.length (tupToList tup))
    then error "extend_env_list: Arguments do not match parameters!"
    else untupHelp ids (tupToList tup) >>+ eval_expr e2


  | _ -> failwith "not implemented"

(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)





let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lexer s =
  let lexbuf = Lexing.from_string s
  in Lexer.read lexbuf


(* Interpret an expression *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_expr
  in run c

