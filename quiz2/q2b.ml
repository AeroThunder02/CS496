(* 
     Quiz 2 - 18 Feb 2022

     Name1: 

     Name2:

 *)

type 'a bt = Empty |  Node of 'a*'a bt*'a bt

(* Helper function that builds a leaf *)
let leaf n = Node(n,Empty,Empty)

(* Two sample binary trees.
   The first one is a BST, but not the second one *)
let ex1 = Node(12,
              Node(7,Empty,leaf 10),
               Node(24,
                    leaf 14,
                    Empty))

let ex2 = Node(12,
              leaf 7,
               Node(24,
                    leaf 30,
                    Empty))

(** returns smallest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec mint t =
  failwith "implement"
  
(** returns largest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec maxt t =
   failwith "implement"

(* Determines whether a binary is a binary search tree *)
let rec is_bst t =
  failwith "implement"

(** adds v to the bst t. 
    Should fail with failwith if v is already in the tree.
    Otherwise, returns updated tree *)
let rec add v t =
  failwith "implement"
           
(* Remove a value from a BST (Extra-credit)
   Should fail (with failwith) if the value is not in tree *)
let rec rem v t =
   failwith "implement"
 





