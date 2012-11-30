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

(* Substract xs from ys *)
let list_substract xs ys =
    match ys with
    | [] -> []
    | head::tail when List.mem head xs -> list_substract xs tail
    | head::tail -> head :: (list_substract xs tail)

(* The set of free variables is the set of let- and function-bindings subtracted
 * from the set of variables.
 * *)
let rec freevars expr =
    match expr with
    (* Base cases *)
    | Num x -> []
    | Bool x -> []
    | Var x -> [x]

    | MonopAp (_, exp) -> freevars exp
    | BinopAp (_, exp1, exp2) -> (freevars exp1) @ (freevars exp2)

    | Cond (if_exp, then_exp, else_exp) ->
        (freevars if_exp) @ (freevars then_exp) @ (freevars else_exp)

    | Fun (var, exp) -> list_substract [var] (freevars exp)
    | FunAp (exp1, exp2) -> (freevars exp1) @ (freevars exp2)

    | Let (var, exp1, exp2) ->
            list_substract [var] ( (freevars exp1) @ (freevars exp2) )

    | LetRec (var, exp1, exp2) ->
            list_substract [var] ( (freevars exp1) @ (freevars exp2) )
