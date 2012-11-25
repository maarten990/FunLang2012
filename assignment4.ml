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


(* Defining the trie datatype *)
type ('a, 'b) trie =
    | Root of ('a * ('a, 'b) trie) list

    (* A node contains a key, value, and list of children *)
    | Node of 'a * 'b * ('a * ('a, 'b) trie) list



let rec trie_insert trie key value =
    match trie with
    | Node (n_key, _, children) -> Node (n_key, Some value, children)

let rec edge_insert edge keys value =
    match edge, keys with
    | (nodekey, trie), [key] when nodekey = key ->
            (nodekey, (trie_insert trie key (value)))

    | (nodekey, Node (_, nodeval, children)), key::keys when nodekey = key ->
            let f = fun x -> edge_insert x keys value in
            (nodekey, Node (nodekey, nodeval, (List.map f children)))

    | (nodekey, Node (_, nodeval, children)), key::keys when nodekey != key ->
            (nodekey, Node (nodekey, nodeval, children))




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
