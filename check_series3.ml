(* Check if your function returns what we expect! *)
let rec checker (f: 'a->'b) (i: ('a * 'b) list): bool list  = 
  match i with
  | [] -> []
  | (inp, out)::xs  -> (compare (f inp) out == 0) :: checker f xs

let isLeapYearCheck = [
  (2012, true);
  (2000, true);
  (1900, false);
  (1960, true);
  (2013, false)
];;

let date2strCheck = [
  ((1,1,1970), "January 1st, 1970");
  ((9,11,1989), "November 9th, 1989");
  ((29,2,2000), "February 29th, 2000");
  ((2,3,2004), "March 2nd, 2004");
  ((11,11,2011), "November 11th, 2011")
];;

let digitRootCheck = [
  (1, 1);
  (10,1);
  (65536,7);
  (12345,6)
];;

let nthCheck = [
  ((0, [1;2;3;4;5;6]), 1);
  ((1, [1;2;3;4;5;6]), 2);
  ((5, [1;2;3;4;5;6]), 6)
];;

let headsCheck = [
  ([[1;2;3];[4;5;6];[7;8;9]], [1;4;7]);
  ([[1];[2;3];[4;5;6;7];[8]], [1;2;4;8])
];;

let pairCheck = [
  (([1;2;3],['a';'b';'c']), [(1,'a');(2,'b');(3,'c')]);
  (([1;2;3],['a';'b']), [(1,'a');(2,'b')]);
  (([2;3],['a';'b';'c']), [(2,'a');(3,'b')])
];;

let isSublistCheck = [
  (([2;3],[1;2;3;4]), true);
  (([1;4],[1;2;3;4]), false);
  (([1;2;3],[1;2;3;4]), true);
  (([1;2;3;4],[1;2;3;4]), true);
  (([3;2;1],[1;2;3;4]), false)
];;

let tokenizeCheck = [
  (("Hello world.!", " ."), ["Hello";"world";"!"]);
  (("a-b-c-d-e-f-g", "-"), ["a";"b";"c";"d";"e";"f";"g"]);
];;
