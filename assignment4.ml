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
            

let rec mergesort x = 
    match x with
    | []    -> []
    | [x]   -> [x]
    | l -> 
            let s1, s2 = half_split l in
            merge (mergesort s1) (mergesort s2)

let rec half_split l = 
    let rec half_split' l s1 length =
        match l with
        | [] -> [], s1
        | x::xs as l when (List.length s1) = length/2 -> List.rev s1, l
        | x::xs -> half_split' xs (x::s1) length
    in

    let length = List.length l in
    half_split' l [] length


(* Defining the trie datatype *)
type ('k, 'v) trie =
    | Leaf
    | Node of 'k * 'v * ('k, 'v) trie list
