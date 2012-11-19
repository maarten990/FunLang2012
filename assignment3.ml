(*
 * Functional Languages assignment 3
 * Maarten de Jonge
 * Inge Becht
 * *)

(* Returns whether a given year is a leapyear or not. A leapyear is defined as
 * either:
 * a year after 1582 divisible by 4 but not by 100
 * a year after 1582 divisible by 200
 * Type: int -> bool
 * *)
let isLeapYear year =
    if (year > 1582 && year mod 4 = 0 && year mod 100 != 0) ||
        year > 1582 && year mod 200 = 0
    then true
    else false

(* Converts a date-tuple to a verbal English string.
 * Type: int * int * int -> string
 * *)
let date2str (day, month, year) =
    let s_year = string_of_int year in
    let digit  = day mod 10 in
    let s_day = string_of_int day ^ if List.mem day [11; 12; 13] then "th" else
        match digit with
        | 1 -> "st"
        | 2 -> "nd"
        | 3 -> "rd"
        | _ -> "th" in
    let months  = ["January"; "February"; "March"; "April"; "May"; "June";
    "July"; "August"; "September"; "October"; "November"; "December"] in
    let s_month = List.nth months (month - 1) in
    s_month ^ " " ^ s_day ^ ", " ^ s_year

(* Gives the digitsum of the given number.
 * Type: int -> int
 * *)
let rec digitSum n =
    if n < 10 then
        n
    else
        let lastDigit = n mod 10 in
        let rest = n / 10 in
        lastDigit + digitSum rest

(* Gives the digitroot of the given number.
 * Type: int -> int
 * *)
let rec digitRoot n =
    if n < 10 then
        n
    else
        digitRoot (digitSum n)

(* Returns the nth element of a list.
 * Type: (int * a list) -> a
 *)

let rec nth (n, l) = match n, l with
    | _, [] -> 0   (* should through an error here or something *)
    | 0, h::_ -> h
    | n, _::t -> nth ((pred n), t)

(* Returns a list of the heads of the given list of lists.
 * Type: a list list -> a list)
 *)
let rec heads l = match l with
    | [] -> []
    | (h::_)::tail -> h :: heads tail
    | []::tail -> 0 :: heads tail (* again, error or something *)

(* Zips 2 lists together in tuples of elements.
 * Type: (a list * b list) -> (a * b) list
 *)
let rec pair (xs, ys) = match xs, ys with
| [], _ -> []
| _, [] -> []
| xh::xt, yh::yt -> (xh, yh) :: pair (xt, yt)

(* Returns true if l is the starting sequence of m
 * Type: (a list * a list) -> bool)
 * *)
let rec sublistBegin (l, m) = 
    match l, m with
    | [], [] -> true
    | _, [] -> false
    | [], _ -> true
    | (lh::lt), (mh::mt) -> if lh = mh then sublistBegin (lt, mt) else false

(* Returns true if l is a substring of m
 * Type: (a list * a list) -> bool)
 * *)
let rec isSublist (l, m) = 
    match m with
    | [] -> false
    | mh::mt -> if sublistBegin (l, m) then true else isSublist (l, mt)

(* Chops the left character off a string
 * Type: string -> string
 * *)
let rec string_lchop s =
    let rec lchop' s n =
        if String.length s = n then
            ""
        else 
            (Char.escaped s.[n]) ^ lchop' s (succ n)
    in lchop' s 1

(* Converts a string into a list of characters.
 *
 * Type: string -> char list
 * *)
let rec list_of_string s =
    if String.length s = 0 then
        []
    else
        (String.get s 0) :: list_of_string (string_lchop s)

(* Converts a list of characters into a string.
 * Type: char list -> string
 * *)
let rec string_of_list = function
    | [] -> ""
    | h::t -> (Char.escaped h) ^ string_of_list t


(* Segments a list of elements c on the given delimiter d)
 * Type: a -> a list -> a list list
 * *)
let rec segment d cs =
    let rec segment' cs d sub = match cs, d, sub with
        | [], _, sub -> [sub]
        | h::t, d, sub -> if h = d then sub::(segment' t d []) else 
            segment' t d (sub @ [h])
    in (segment' cs d [])

(* Tokenizes a list of "words" in the form of character lists, using a list of
 * delimiters.
 * Type: a list list -> a list -> a list list
 * *)
let rec tokenize' cs ds =
    match cs, ds with
    | cs, [] -> cs
    | cs, d::ds ->
            let step = List.flatten (List.map (segment d) cs) in
            tokenize' step ds

(* Tokenizes the string c on the delimiters given in the string ds
 * Type: string * string -> string list
 * *)
let tokenize (c, ds) = 
    let c_list = list_of_string c in
    let ds_list = list_of_string ds in
    List.map string_of_list (tokenize' [c_list] ds_list);;



(***************************************************************************
*                                Challenge                                *
***************************************************************************)

(* Returns the count of an element in a list *)
let rec count elem xs =
    match elem, xs with
    | elem, [] -> 0
    | elem, x::xs ->
            if elem = x then
                1 + count elem xs
            else
                count elem xs

(* Checks if the first list is a permutation of the second list *)
let checkPermute xs target =
    let bools = List.map (fun a -> (count a xs) = (count a target)) xs in
    List.for_all (fun a -> a) bools

(* Given a list of lists, filter it such that no element of the list is a
 * permutation of any other element *)
let rec filterPermutes = function
    | [] -> []
    | x::xs -> 
            if List.exists (fun a -> checkPermute x a) xs then
                filterPermute xs
            else
                x::(filterPermute xs)
        
(* expands a node in the search tree *)
let rec expand possibilities combination =
    match possibilities, combination with
    | [], _ -> []
    | h::t, comb -> (h::comb) :: (expand t comb)

(* performs a depth-first search to find all possible ways of attaining the goal
 * given the possible coin values. 
 * 'value', 'combination' and 'visited' assumed to be passed as 0, [] and [].
 * (Sloppy, yes. But I really need to sleep right now! *)
let rec depthFirst possibilities goal value combination visited =
    if value = goal then
        [combination]
    else if value < goal then
        let f ncomb = depthFirst possibilities goal (value + (List.hd
            ncomb)) ncomb (combination::visited) in
        filterPermutes (List.concat (List.map f (expand possibilities
        combination)))
    else
        []

(* Returns a tuple whose first element is the number of ways to get the value
 * 'n' using the given coin denominations, and whose second element is a list
 * containing said ways.
 *
 * Is incredibly slow when there's too many options. A depthfirst search might
 * not be the best algorithm for this, but it's easy.
 * *)
let rec coinPermute coins n = 
    let options = depthFirst coins n 0 [] [] in
    (List.length options, options)
