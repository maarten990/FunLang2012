type monop = Neg | Not
type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or

type expr =
  Num of int
| Bool of bool
| Var of string
| MonopAp of monop * expr
| BinopAp of binop * expr * expr
| Cond of expr * expr * expr
| Fun of string * expr
| FunAp of expr * expr
| Let of string * expr * expr
| LetRec of string * expr * expr

let monop2string op =
    match op with
    | Neg -> "-"
    | Not -> "!"

let binop2string op =
    match op with
    | Add -> " + "
    | Sub -> " - "
    | Mul -> " * "
    | Div -> " / "
    | Mod -> " mod "
    | Eq  -> " = "
    | Ne  -> " != "
    | Lt  -> " < "
    | Le  -> " <= "
    | Gt  -> " > "
    | Ge  -> " >= "
    | And -> " && "
    | Or  -> " || "

let rec expr2string expr =
    match expr with
    | Num x -> string_of_int x
    | Bool x -> string_of_bool x
    | Var x -> x

    | MonopAp (op, expr) -> (monop2string op) ^ (expr2string expr)
    | BinopAp (op, expr1, expr2) ->
            (expr2string expr1) ^ (binop2string op) ^ (expr2string expr2)

    | Cond (if_exp, then_exp, else_exp) ->
            "if " ^ (expr2string if_exp) ^ " then " ^ (expr2string then_exp) ^
            " else " ^ (expr2string else_exp)

    | Fun (var, exp) -> "fun " ^ var ^ " -> " ^ (expr2string exp)
    | FunAp (exp1, exp2) -> "(" ^ (expr2string exp1) ^ " " ^ (expr2string exp2) ^ ")"

    | Let (var, exp1, exp2) ->
            "let " ^ var ^ " = " ^ (expr2string exp1) ^ " in "
            ^ (expr2string exp2)
    | LetRec (var, exp1, exp2) ->
            "let rec " ^ var ^ " = " ^ (expr2string exp1) ^ " in "
            ^ (expr2string exp2)

(* The set of free variables is the set of let- and function-bindings subtracted
 * from the set of variables.
 * *)
let rec freevars expr =

    (* Substract xs from ys *)
    let rec list_substract xs ys =
        match ys with
        | [] -> []
        | head::tail when List.mem head xs -> list_substract xs tail
        | head::tail -> head :: (list_substract xs tail)
    in

    (* Infix operator that appends 2 lists while removing duplicates *)
    let rec (@@) xs ys =
        match xs with
        | [] -> ys
        | h::t when List.mem h ys -> t @@ ys
        | h::t -> h::(t @@ ys)
    in

    match expr with
    (* Base cases *)
    | Num x -> []
    | Bool x -> []
    | Var x -> [x]

    | MonopAp (_, exp) -> freevars exp
    | BinopAp (_, exp1, exp2) -> (freevars exp1) @@ (freevars exp2)

    | Cond (if_exp, then_exp, else_exp) ->
        (freevars if_exp) @@ (freevars then_exp) @@ (freevars else_exp)

    | Fun (var, exp) -> list_substract [var] (freevars exp)
    | FunAp (exp1, exp2) -> (freevars exp1) @@ (freevars exp2)

    | Let (var, exp1, exp2) ->
            list_substract [var] ( (freevars exp1) @@ (freevars exp2) )

    | LetRec (var, exp1, exp2) ->
            list_substract [var] ( (freevars exp1) @@ (freevars exp2) )

(* Substitute all free occurences of var in exp with sub_expr *)
let rec subs var expr sub_expr =
    let sub = fun x -> subs var x sub_expr in

    match expr with
    | Var x when x = var -> sub_expr

    | MonopAp (op, exp) -> MonopAp (op, sub exp)
    | BinopAp (op, exp1, exp2) -> BinopAp (op, sub exp1, sub exp2)

    | Cond (i, t, e) -> Cond (sub i, sub t, sub e)
    | FunAp (exp1, exp2) -> FunAp (sub exp1, sub exp2)

    (* Don't substitute in the body of a function if it binds the variable to be
     * substituted. *)
    | Fun (bound_var, exp) when bound_var = var -> Fun (bound_var, exp)
    | Fun (bound_var, exp) -> Fun (bound_var, sub exp)

    (* Don't evaluate the second expression of a let expression if the variable
     * gets bound. *)
    | Let (bound_var, exp1, exp2) when bound_var = var ->
            Let (bound_var, sub exp1, exp2)
    | LetRec (bound_var, exp1, exp2) when bound_var = var ->
            Let (bound_var, sub exp1, exp2)

    | Let (bound_var, exp1, exp2) ->
            Let (bound_var, sub exp1, sub exp2)
    | LetRec (bound_var, exp1, exp2) ->
            Let (bound_var, sub exp1, sub exp2)

    | other_thing -> other_thing


(*Assignment 13: Red-Black Tree*)
type color = 
    | Red
    | Black

type 'a rbtree = 
    | Node of color * 'a * 'a rbtree * 'a rbtree
    | Leaf


(*The balance and instert function as given in the boo k *)
(*color * 'a * 'a rbtree * 'a rbtree -> 'a rbtree*)
let balance = function 
    | Black, z, Node (Red, y , Node (Red, x, a, b), c), d
    | Black, z, Node (Red, x , a, Node (Red, y, b, c)), d
    | Black, x, a, Node (Red, z , Node (Red, y, b, c), d)
    | Black, x, a, Node (Red, y , b, Node( Red, z, c, d )) ->
            Node (Red, y, Node (Black, x,a,b), Node (Black, z, c, d))
    | a,b,c,d ->
            Node (a,b,c,d)

(*'a -> 'a rbtree -> 'a rbtree*)
let insert x s = 
    let rec ins = function
        Leaf -> Node(Red, x, Leaf, Leaf)
    | Node (color, y, a, b) as s ->
            if x < y then balance (color, y, ins a, b)
            else if x > y then balance (color, y, a, ins b)
            else s
    in
        match ins s with
    
    | Node (_, y, a, b) -> Node (Black, y, a, b)
    | Leaf -> raise (Invalid_argument "insert")


(* Lookup*) 
(* 'a rbtree -> 'a -> bool*)
let rec lookup rbtree value = 
    match rbtree with
    | Leaf -> false
    | Node(_, x, _, _) when x = value ->  true
    | Node(_, x, a, _) when value < x -> lookup a value
    | Node(_, _, _, b) -> lookup b value

