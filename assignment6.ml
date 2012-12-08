(* Functional Languages assignment 6
 * Inge Becht
 * Maarten de Jonge
 * *)

(* The Trie module *)
module Trie :
    (* the Trie module's public interface *)
    sig
        type ('a, 'b) trie
        val empty : ('a, 'b) trie
        val insert : ('a, 'b option) trie -> 'a list -> 'b -> ('a, 'b option) trie
        val remove : ('a, 'b option) trie -> 'a list -> ('a, 'b option) trie
        val removeAll : ('a, 'b option) trie -> 'a list -> ('a, 'b option) trie
        val lookup : (char, 'a option) trie -> char list -> 'a option
    end
    =
    struct
    (* The trie datatype *)
    type ('a, 'b) trie =
        | Root of ('a, 'b) trie list

        (* A node contains a key, value, and list of children *)
        | Node of 'a * 'b * (('a, 'b) trie) list

    (* Defining a special constant for the empty trie allows hiding of the
     * datatype *)
    let empty = Root []

    (* Inserts a key (as list of elements) and value into a trie Node *)
    let rec insert_node node keys value =

        (* Returns the keys of a list of nodes *)
        let rec child_keys = function
            | [] -> []
            | Node(key, _, _) :: rest -> key::(child_keys rest)
        in

        match node, keys with
        (* If there's one key and it matches the current node's key, just change its
         * value *)
        | Node (nodekey, nodeval, children), [key] when nodekey = key ->
                    Node (nodekey, Some value, children)

        (* If there's one key and it doesn't match the node, return the node. *)
        | Node (nodekey, nodeval, children), [key] ->
                    Node (nodekey, nodeval, children)

        (* If there are multiple keys and the first one matches the current node:
            * If the next key is already present, apply insert_node to every
            * child node.
            *
            * If the next key isn't present, add a new node with that key and no
            * value to the children, and apply insert_node to every child node.
            * *)
        | Node (nodekey, nodeval, children), key::nextkey::keys when nodekey = key ->
                if not (List.mem nextkey (child_keys children)) then
                    let newchildren = Node (nextkey, None, []) :: children in
                    let f = fun x -> insert_node x (nextkey::keys) value in
                    Node (nodekey, nodeval, (List.map f newchildren))
                else
                    let f = fun x -> insert_node x (nextkey::keys) value in
                    Node (nodekey, nodeval, (List.map f children))

        (* If none of the previous cases are true, just return the node *)
        | node, _ -> node


    (* Inserts a key (as list of elements) and value into a trie Root *)
    (*Expects a Root*)
    let insert root keys value =
        let rec child_keys = function
            | [] -> []
            | Node(key, _, _) :: rest -> key::(child_keys rest)
        in

        match root, keys with
        | Root children, key::keys ->
                if not (List.mem key (child_keys children)) then
                    let newchildren = Node (key, None, []) :: children in
                    let f = fun x -> insert_node x (key::keys) value in
                    Root (List.map f newchildren)
                else
                    let f = fun x -> insert_node x (key::keys) value in
                    Root (List.map f children)



    let rec remove' nodes key = 
       match nodes with 
       (*In case last element of key matches with head key of node return only the
        * tail*)
       | Node(key', _, [])::t when key' = (List.hd key) && (List.length key) <=
           1 -> t
       (* In case last element of key match with head but has children just return
        * the whole node section with head value set to None*)
       | Node(key', value, children)::t when key' = (List.hd key) && (List.length key) <=
           1 -> Node(key', None, children)::t 
       (* In case not last element of key matches go into recursion*)
       | Node(key', value, children)::t when key' = (List.hd key) ->
               Node(key', value, remove' children (List.tl key))::t
       (*Head does not match, recurse with t*)
       | h::t -> h::remove' t key
       | _ -> []

    (*Removes a key from a trie and returns this new trie. In case key not in trie
     * returns same trie as input*)
    (*Expects a Root*)
    let remove trie key = 
        match trie with 
        | Root(nodes) -> Root(remove' nodes key)


    let rec removeAll' nodes key = 
       match nodes with 
       (*In case last element of key matches with head key of node return only the
        * tail*)
       | Node(key', _, _)::t when key' = (List.hd key) && (List.length key) <=
           1 -> t
       (* In case not last element of key matches go into recursion*)
       | Node(key', value, children)::t when key' = (List.hd key) ->
               Node(key', value, removeAll' children (List.tl key))::t
       (*Head does not match, recurse with t*)
       | h::t -> h::removeAll' t key

    (*Removes all nodes under the given key. If it does not resist it just returns
     * the input Trie*)
    (*Expects a Root*)
    let removeAll trie key = 
        match trie with 
        | Root(nodes) -> Root(removeAll' nodes key)



    let rec lookup' nodes key = 
        print_char (List.hd key);
        match nodes with 
        (*In case no nodes the key does not exist*)
        | [] -> None
        (*Last element of key is the same as key of head of the nodes; return
         * value*)
        | Node(key', value, children)::_ when ((List.length key) <= 1) && key' =
           ( List.hd key ) -> value
        (*Look in the children of the head of the list*)
        | Node(key', value, children)::_ when key' = (List.hd key) -> lookup'
        children (List.tl key)
        (*Recursion into tail*)
        | _::t -> lookup' t key

    (*Looks up a value given a key. Returns None if key none existent*)
    (*Expects a Root*)
    let lookup trie key = 
        match trie with
        | Root(nodes) -> lookup' nodes key
    end






(* Lambda calculus stuff starts here, starting with the necessary definitions
 * and functions from the previous assignment. *)




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

(* Converts an expression to an OCaml string representation *)
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

    (* Infix operator that appends 2 lists while removing duplicates, used to
     * avoid non-sensical return values such as ["a"; "a"; "b"] *)
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

(* Checks whether a given expression is in normal form, i.e. doesn't contain any
 * function applications *)
let rec isNormalForm exp =
    match exp with
    | Var _
    | Num _
    | Bool _ -> true

    | FunAp (_, _) -> false

    | MonopAp (_, exp)
    | Fun (_, exp) -> isNormalForm exp

    | BinopAp (_, exp1, exp2)
    | Let (_, exp1, exp2)
    | LetRec (_, exp1, exp2) -> isNormalForm exp1 && isNormalForm exp2

    | Cond (exp1, exp2, exp3) ->
            isNormalForm exp1 && isNormalForm exp2 && isNormalForm exp3

(* Reduces an expression to normalform using applicative order *)
let rec evalApplicative exp =
    if isNormalForm exp then
        exp
    else
        match exp with
        (* When dealing with a function application, get the argument into
         * normalform first *)
        | FunAp ( Fun (var, exp), arg ) -> 
                let normalArg = evalApplicative arg in
                evalApplicative (subs var exp normalArg)

        | FunAp (func, arg) -> FunAp (evalApplicative func, evalApplicative arg)

        | MonopAp (op, exp) -> MonopAp (op, evalApplicative exp)
        | BinopAp (op, exp1, exp2) ->
                BinopAp (op, evalApplicative exp1, evalApplicative exp2)

        | Cond (if_exp, then_exp, else_exp) ->
                Cond (evalApplicative if_exp, evalApplicative then_exp,
                evalApplicative else_exp)

        | Fun (var, exp) -> Fun (var, evalApplicative exp)

        | Let (var, exp1, exp2) ->
                Let (var, evalApplicative exp1, evalApplicative exp2)

        | LetRec (var, exp1, exp2) ->
                LetRec (var, evalApplicative exp1, evalApplicative exp2)

        | atomic -> atomic

(* Reduces an expression to normalform using normal order *)
let rec evalNormal exp =
    if isNormalForm exp then
        exp
    else
        match exp with
        | FunAp ( Fun (var, exp), arg ) -> 
                evalNormal (subs var exp arg)

        | FunAp (func, arg) -> FunAp (evalNormal func, evalNormal arg)

        | MonopAp (op, exp) -> MonopAp (op, evalNormal exp)
        | BinopAp (op, exp1, exp2) ->
                BinopAp (op, evalNormal exp1, evalNormal exp2)

        | Cond (if_exp, then_exp, else_exp) ->
                Cond (evalNormal if_exp, evalNormal then_exp,
                evalNormal else_exp)

        | Fun (var, exp) -> Fun (var, evalNormal exp)

        | Let (var, exp1, exp2) ->
                Let (var, evalNormal exp1, evalNormal exp2)

        | LetRec (var, exp1, exp2) ->
                LetRec (var, evalNormal exp1, evalNormal exp2)

        | atomic -> atomic
