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
(*
let insert trie key value =
    match trie with
    | Root [] -> Root [Node (key, value, [])]
*)



let rec remove' nodes key = 
    match nodes with
    | [] -> []
    (* In case we have a list of nodes do pattern matching to find*)
    | (h, Node(_, _, children))::t when h = List.hd key && (List.length key) > 1 ->
              (remove' children (List.tl key))::t
    (* Head is the thing to remove*)
    | (h, Node(_, _, _))::t when h = (List.hd key) -> t
    | h::t -> h::(remove' [] key)

(*Root( [Node('a', None, [] ; Node()]) *)
(* Remove key from trie*)
let rec remove trie key =
    match trie with
    (* In case trie is empty tree return trie*)
    | Root([]) -> trie
    (* In case Trie is at root node go into recursion*)
    | Root(nodes) -> Root([remove' nodes key])



