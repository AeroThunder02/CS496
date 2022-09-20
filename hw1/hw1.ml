(* Maciej Kowalczyk *)
(* I pledge my Honor that I have abided by the Stevens Honor System *)
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let rec cmb_list l1 l2 =
  match l1 with
  |[] -> l2
  |h::t -> if List.mem h l2
    then cmb_list t l2
    else h::cmb_list t l2

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [] 

let check fa =
  match fa.final with
  |[] -> false
  |h::_ -> if List.mem h fa.states && List.mem fa.start fa.states
    then true
    else false

let rec remove_state state keep =
  match state with
  |[] -> []
  |h::t -> if List.mem h keep
    then h::remove_state t keep
    else remove_state t keep

let rec clean_tf state keep =
  match state with
  |[] -> []
  |(x,y,x_tgt)::tf_t -> if List.mem x keep
    then (x,y,x_tgt)::clean_tf tf_t keep
    else clean_tf tf_t keep    

(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

    
let rec apply_transition_function tf state symbol =
  match tf with
  | [] -> None
  | (x,y,tgtx)::tf_t when x = state && y = symbol -> Some tgtx
  | _::tf_t -> apply_transition_function tf_t state symbol


let rec accept fa input =
  let rec sym_match state input= 
    match input with 
    |[] -> state
    |h::t -> 
      match state with 
      |Some q -> sym_match(apply_transition_function fa.tf q h) t
      |None -> None
  in match sym_match(Some fa.start) input with
  |None -> false
  |Some state -> List.mem state fa.final

let rec next tf state symbol = 
  match tf with 
  |[] -> []
  |(x,y,tgt_x)::tf_t -> if x = state && y = symbol 
    then tgt_x::next tf_t state symbol
    else next tf_t state symbol

let rec deterministic fa = 
  let rec check_tf tf = 
    match tf with
    |[] -> true
    |(x,y,tgt_x)::tf_tail -> 
      match (next tf_tail x y) with
      |[] -> check_tf tf_tail
      |_ -> false
  in check_tf fa.tf  

let rec valid fa =
  if check fa && deterministic fa
  then true
  else false


let rec reachable fa = 
  let rec find_reach tf start states =
    match tf with  
    |[] -> []
    |(x,y,tgt_x)::tf_t -> if x = start && x<>tgt_x
    then if List.mem tgt_x states
      then find_reach tf_t start states
      else tgt_x::find_reach tf_t start (tgt_x::states)
    else find_reach tf_t start states
  in let rec combine state tgt =
    match state with
    |[] -> [fa.start]
    |h::t -> if List.mem h tgt
      then combine t tgt
      else h::combine(cmb_list(find_reach fa.tf h(List.append state tgt)) t) (List.append [h] tgt)
  in combine (find_reach fa.tf fa.start []) [fa.start]
 

let rec remove_dead_states fa =
  let reachable_states = reachable fa in {
    states = reachable_states;
    tf = clean_tf fa.tf reachable_states;
    start = fa.start;
    final = remove_state fa.final reachable_states;
  }

