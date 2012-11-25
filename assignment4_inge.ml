
(* Defining the trie datatype *)
type ('a, 'b) trie =
    | Root of ('a * ('a, 'b) trie) list

    (* A node contains a key, value, and list of children *)
    | Node of 'a * 'b * ('a * ('a, 'b) trie) list


let rec insert' nodes key value = 
    match nodes with
    (* in case no more nodes add to layer*)
    | [] -> [insert_value key value]
    | (h, Node(foo, bar, children))::t when h = (List.hd key) ->
              (h, Node(foo, bar, (insert' children (List.tl key) value)))::t
    | h::t -> h::(insert' t key value)


let rec insert_value key value = 
    if (List.length key) = 1 then (List.hd key, Node (List.hd key, value, []))
    else
        (List.hd key, Node(List.hd key, None,  [insert_value (List.tl key) value]))

let insert trie key value = 
    match trie with 
    | Root(nodes) -> Root(insert' nodes key value)
