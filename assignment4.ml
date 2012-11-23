let rec quicksort = function
    | [] -> []
    | x::xs ->
        let isSmaller = fun a -> a < x in
        let smaller, largerEq = List.partition isSmaller xs in
        quicksort smaller @ [x] @ quicksort largerEq

(* Defining the trie datatype *)
type ('k, 'v) trie =
    | Leaf
    | Node of 'k * 'v * ('k, 'v) trie list
