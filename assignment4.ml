let rec quicksort = function
    | [] -> []
    | x::xs ->
        let isSmaller = fun a -> a < x in
        let smaller, largerEq = List.partition isSmaller xs in
        quicksort smaller @ [x] @ quicksort largerEq

(* Defining the trie datatype *)
type ('a, 'b) trie =
    | Node of 'a * 'b * (('a, 'b) trie) list
