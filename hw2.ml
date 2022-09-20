(*I pledge my Honor that I have abided by the Stevens Honor System*)
type dtree =
 |Leaf of int
 |Node of (char * dtree * dtree)

let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))
let dBin = Node('x', Node('y', Node('y', Leaf(0), Leaf(0)), Node('y', Leaf(0), Leaf(0))), Node('y', Node('y', Leaf(0), Leaf(0)), Leaf(0)))

let rec dTree_height t =
 match t with
 |Leaf(x) -> 0
 |Node(c, lt, rt) ->
  let l = dTree_height lt in
  let r = dTree_height rt in
  if l > r then l + 1 else r + 1

let rec dTree_size t =
 match t with
 |Leaf(x) -> 1
 |Node(c, lt, rt) -> 1 + (dTree_size lt) + (dTree_size rt)

let rec dTree_paths t =
 let leftpath lst = 0::lst in
 let rightpath lst = 1::lst in
 match t with
 |Leaf(x) -> [[]]
 |Node(c, lt, rt) -> (List.map leftpath (dTree_paths lt)) @ (List.map rightpath (dTree_paths rt))

let rec dTree_is_perfect t =
 match t with
 |Leaf(x) -> true
 |Node(c, lt, rt) ->
 let l_height = dTree_height lt in
 let r_height = dTree_height rt in
 if l_height != r_height then false
 else (dTree_is_perfect lt) && (dTree_is_perfect rt)

let rec dTree_map f g t =
 match t with
 |Leaf(x) -> Leaf(g x)
 |Node(c, lt, rt) -> Node(f c, (dTree_map f g lt), (dTree_map f g rt))

let rec list_to_tree lst =
 match lst with
 |[] -> Leaf(0)
 |h::t ->
  match h with
  |lst -> Node(h, (list_to_tree t), (list_to_tree t))

(*TA's help*)
let rec replace_helper x y tree = 
  match tree with 
  |Leaf(some_value) -> Leaf(y)
  |Node(c, lt, rt) -> 
  if List.hd x = 0
  then Node(c, (replace_helper x y lt), rt)
  else Node(c, lt, (replace_helper x y rt))
  
let rec replace_leaf_at t g = 
  match g with
  |[] -> t
  |(x,y)::tail ->  
  replace_leaf_at (replace_helper x, y, tail) g 

let rec bf_to_dTree (x,y) =
  replace_leaf_at (list_to_tree x) y  
