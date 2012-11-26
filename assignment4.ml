(*Assignment 9*)
let rec quicksort = function
    | [] -> []
    | x::xs ->
        let isSmaller = fun a -> a < x in
        let smaller, largerEq = List.partition isSmaller xs in
        quicksort smaller @ [x] @ quicksort largerEq


let rec merge a b =
    match a, b with
    | [], x
    | x, [] -> x
    | xh::xt, yh::yt when xh <= yh ->  
                xh::(merge xt (yh::yt))
    | x, yh::yt -> yh::(merge x yt)
            
let rec half_split l = 
    let rec half_split' l s1 length =
        match l with
        | [] -> [], s1
        | x::xs as l when (List.length s1) = length/2 -> List.rev s1, l
        | x::xs -> half_split' xs (x::s1) length
    in

    let length = List.length l in
    half_split' l [] length


let rec mergesort x = 
    match x with
    | []    -> []
    | [x]   -> [x]
    | l -> let s1, s2 = half_split l in
            merge (mergesort s1) (mergesort s2)


(* The trie datatype *)
type ('a, 'b) trie =
    | Root of ('a, 'b) trie list

    (* A node contains a key, value, and list of children *)
    | Node of 'a * 'b * (('a, 'b) trie) list


(* Inserts a key (as list of elements) and value into a trie Node *)
let rec insert_node node keys value =

    (* Returns the keys of a list of nodes *)
    let rec child_keys = function
        | [] -> []
        | Node(key, _, _) :: rest -> key::(child_keys rest)
    in

    match node, keys with
    (* If there's one key and it matches the current node's key, just change its
     * value *)
    | Node (nodekey, nodeval, children), [key] when nodekey = key ->
                Node (nodekey, Some value, children)

    (* If there's one key and it doesn't match the node, return the node. *)
    | Node (nodekey, nodeval, children), [key] ->
                Node (nodekey, nodeval, children)

    (* If there are multiple keys and the first one matches the current node:
        * If the next key is already present, apply insert_node to every
        * child node.
        *
        * If the next key isn't present, add a new node with that key and no
        * value to the children, and apply insert_node to every child node.
        * *)
    | Node (nodekey, nodeval, children), key::nextkey::keys when nodekey = key ->
            if not (List.mem nextkey (child_keys children)) then
                let newchildren = Node (nextkey, None, []) :: children in
                let f = fun x -> insert_node x (nextkey::keys) value in
                Node (nodekey, nodeval, (List.map f newchildren))
            else
                let f = fun x -> insert_node x (nextkey::keys) value in
                Node (nodekey, nodeval, (List.map f children))

    (* If none of the previous cases are true, just return the node *)
    | node, _ -> node


(* Inserts a key (as list of elements) and value into a trie Root *)
let insert root keys value =
    let rec child_keys = function
        | [] -> []
        | Node(key, _, _) :: rest -> key::(child_keys rest)
    in

    match root, keys with
    | Root children, key::keys ->
            if not (List.mem key (child_keys children)) then
                let newchildren = Node (key, None, []) :: children in
                let f = fun x -> insert_node x (key::keys) value in
                Root (List.map f newchildren)
            else
                let f = fun x -> insert_node x (key::keys) value in
                Root (List.map f children)



let rec lookup' nodes key = 
    match nodes with 
    | [] -> None
    | Node(key', value, children)::_ when ((List.length key) <= 1) && key' =
       ( List.hd key ) -> value
    | Node(key', value, children)::_ when key' = (List.hd key) -> lookup' children key
    | _::t -> lookup' t key

let lookup trie key = 
    match trie with
    | Root(nodes) -> lookup' nodes key




(* assignment 11 *)
(* Types could be added to the operators
 * (such as IntConst -> IntConst -> IntConst for the arithmetic operatators),
 * but I'm not sure how useful that would be. *)
type arithOp = Plus | Minus | Mult | Div | Mod
type relOp = Eq | Neq | LessThan | LessEq | GreaterThan | GreaterEq
type logicOp = And | Or
type monOp = Neg | Not
type const =
    | BoolConst of bool
    | IntConst of int

type binOp =
    | ArithOp of arithOp
    | RelOp of relOp
    | BinOp of binOp

type expr =
    | Expr of expr
    | BinRel of expr * binOp * expr
    | MonRel of monOp * expr
    | Id
    | Const of const
