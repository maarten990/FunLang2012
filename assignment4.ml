(* Assignment 4
 * By Inge Becht #6093906
 *   Maarten de Jonge #6286658*)
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
(*Expects a Root*)
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




let rec remove' nodes key = 
   match nodes with 
   (*In case last element of key matches with head key of node return only the
    * tail*)
   | Node(key', _, [])::t when key' = (List.hd key) && (List.length key) <=
       1 -> t
   (* In case last element of key match with head but has children just return
    * the whole node section with head value set to None*)
   | Node(key', value, children)::t when key' = (List.hd key) && (List.length key) <=
       1 -> Node(key', None, children)::t 
   (* In case not last element of key matches go into recursion*)
   | Node(key', value, children)::t when key' = (List.hd key) ->
           Node(key', value, remove' children (List.tl key))::t
   (*Head does not match, recurse with t*)
   | h::t -> h::remove' t key
   | _ -> []

(*Removes a key from a trie and returns this new trie. In case key not in trie
 * returns same trie as input*)
(*Expects a Root*)
let remove trie key = 
    match trie with 
    | Root(nodes) -> Root(remove' nodes key)


let rec removeAll' nodes key = 
   match nodes with 
   (*In case last element of key matches with head key of node return only the
    * tail*)
   | Node(key', _, _)::t when key' = (List.hd key) && (List.length key) <=
       1 -> t
   (* In case not last element of key matches go into recursion*)
   | Node(key', value, children)::t when key' = (List.hd key) ->
           Node(key', value, removeAll' children (List.tl key))::t
   (*Head does not match, recurse with t*)
   | h::t -> h::removeAll' t key

(*Removes all nodes under the given key. If it does not resist it just returns
 * the input Trie*)
(*Expects a Root*)
let removeAll trie key = 
    match trie with 
    | Root(nodes) -> Root(removeAll' nodes key)



let rec lookup' nodes key = 
    print_char (List.hd key);
    match nodes with 
    (*In case no nodes the key does not exist*)
    | [] -> None
    (*Last element of key is the same as key of head of the nodes; return
     * value*)
    | Node(key', value, children)::_ when ((List.length key) <= 1) && key' =
       ( List.hd key ) -> value
    (*Look in the children of the head of the list*)
    | Node(key', value, children)::_ when key' = (List.hd key) -> lookup'
    children (List.tl key)
    (*Recursion into tail*)
    | _::t -> lookup' t key

(*Looks up a value given a key. Returns None if key none existent*)
(*Expects a Root*)
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
