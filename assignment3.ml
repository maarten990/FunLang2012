(* Returns whether a given year is a leapyear or not. A leapyear is defined as
 * either:
 * a year after 1582 divisible by 4 but not by 100
 * a year after 1582 divisible by 200
 *
 * Type: int -> bool
 * *)
let isLeapYear year =
    if (year > 1582 && year mod 4 = 0 && year mod 100 != 0) ||
        year > 1582 && year mod 200 = 0
    then true
    else false

(* Converts a date-tuple to a verbal English string.
 *
 * Type: int * int * int -> string
 * *)
let date2str (day, month, year) =
    let s_year = string_of_int year in
    let digit  = day mod 10 in
    let s_day = string_of_int day ^ match digit with
    | 1 -> "st"
    | 2 -> "nd"
    | 3 -> "rd"
    | _ -> "th" in
    let months  = ["January"; "Februari"; "March"; "April"; "May"; "April";
    "June"; "July"; "August"; "September"; "October"; "November"; "December"] in
    let s_month = List.nth months (month - 1) in
    s_month ^ " " ^ s_day ^ ", " ^ s_year

(* Gives the digitroot of the given number.
 *
 * Type: int -> int
 * *)
let rec digitRoot n =
    if n < 10 then
        n
    else
        let digitSum = (n / 10) + (n mod 10) in
        digitRoot digitSum

(* Returns the nth element of a list.
 *
 * Type: int -> a list -> a
 *)

let rec nth n l = match n, l with
    | 0, h::_ -> h
    | n, _::t -> nth (pred n) t

(* Returns a list of the heads of the given list of lists.
 *
 * Type: a list list -> a list)
 *)
let rec heads l = match l with
    | [] -> []
    | (h::_)::tail -> h :: heads tail

(* Zips 2 lists together in tuples of elements.
 *
 * Type: a list -> b list -> a * b
 *)
let rec pairs xs ys = match xs, ys with
| [], _ -> []
| _, [] -> []
| xh::xt, yh::yt -> (xh, yh) :: pairs xt yt


let rec sublistBegin l m = 
    match l, m with
    | _, [] -> false
    | [], _ -> true
    | (lh::lt), (mh::mt) -> if lh = mh then subListBegin lt mt else false

let rec isSublist l m = 
    match m with
    | [] -> false
    | mh::mt -> if sublistBegin l m then true else isSublist l mt

(*
let rec list_of_string s =
    if String.isempty s then
        []
    else
        (String.get s 0) :: list_of_string (String.lchop s)

let tokenize c d =
    let c_list = list_of_string c in
    let d_list = list_of_string d in
    4
*)
