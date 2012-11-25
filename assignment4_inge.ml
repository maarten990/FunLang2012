
(* Defining the trie datatype *)
type ('a, 'b) trie =
    | Root of ('a * ('a, 'b) trie) list

    (* A node contains a key, value, and list of children *)
    | Node of 'a * 'b * ('a * ('a, 'b) trie) list
    
let rec insert' nodes key value = 
    match nodes with
    (* in case no more nodes add to layer*)
    | [] -> (key, Node(key, value, []))


let rec insert_value nodes key value = 
    if List.length key = 1 then (key, Node (key, value, []))
    else
        (key, Node(List.hd key, None, (insert_value nodes (List.tl key) value)))


let insert trie key value = 
    match trie with 
    | Root(nodes) -> Root(insert' nodes key value)

