(*

Name:
  Email:
    Minutes Spent on Problem 2:

      (You aren't in any way graded on the number of minutes spent;
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:

  *)

open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
  * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionlibrary.ml
 * 3. Test!  (Use "assert" where appropriate.)
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
  match e with
  | Num n -> false
  | Var -> true
  | Binop (b, e1, e2) -> contains_var(e1) || contains_var(e2)
  | Unop (u, e1) -> contains_var(e1)
;;

assert (contains_var (parse "x^4"));;
assert (not (contains_var (parse "4+3")));;


(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Use OCaml's
 *            built in method of handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  | Num n -> n
  | Var -> x
  | Binop (b, e1, e2) -> (
    let s1 = evaluate e1 x in
    let s2 = evaluate e2 x in
    match b with
      | Add -> s1 +. s2
      | Sub -> s1 -. s2
      | Mul -> s1 *. s2
      | Div -> s1 /. s2
      | Pow -> s1 ** s2)
  | Unop (u, e1) -> (
    let s1 = evaluate e1 x in
    match u with
      | Sin -> sin s1
      | Cos -> cos s1
      | Ln -> log s1
      | Neg -> 0. -. s1)
;;

assert ((evaluate (parse "x^4+3") 2.0) = 19.0);;


(*>* Problem 2.3 *>*)

(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with
  | Num n -> Num 0.
  | Var -> Num 1.
  | Binop (b, e1, e2) -> (
    let d1 = derivative e1 in
    let d2 = derivative e2 in
    match b with
    | Add -> Binop (Add, d1, d2)
    | Sub -> Binop (Sub, d1, d2)
    | Mul -> Binop (Add,
                Binop (Mul, d1, e2),
                Binop (Mul, e1, d2))
    | Div -> Binop (Div,
                Binop (Sub,
                 Binop (Mul, d1, e2),
                 Binop (Mul, e1, d2)),
                Binop (Pow, d2, Num 2.))
    | Pow ->
        if not (contains_var e2)
        then Binop (Mul, e2, Binop (Mul, d1, Binop (Pow,
                         e1, Binop (Sub, e2, Num 1.))))
        else Binop (Mul,
                Binop (Pow, e1, e2),
                Binop (Add,
                 Binop (Mul, d2, Unop (Ln, e1)),
                 Binop (Div,
                  Binop (Mul, d1, e2),
                  e1))))
    | Unop (u, e1) -> (
      let d1 = derivative e1 in
      match u with
      | Sin -> Binop (Mul, d1, Unop (Cos, e1))
      | Cos -> Binop (Mul, Unop (Neg, d1), Unop (Sin, e1))
      | Ln -> Binop (Mul, d1, Binop (Div, Num 1.0, e1))
      | Neg -> Unop (Neg, d1))
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval=
  print_string ("Checking expression: " ^ strs^"\n");
  let parsed = parse strs in (
    print_string "contains variable : ";
  print_string (string_of_bool (contains_var parsed));
  print_endline " ";
  print_string "Result of evaluation: ";
  print_float  (evaluate parsed xval);
  print_endline " ";
  print_string "Result of derivative: ";
  print_endline " ";
  print_string (to_string (derivative parsed));
  print_endline " ");;


(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
: float option =
  if (abs_float (evaluate e g)) < epsilon then Some g
  else if lim <= 0 then None
  else let e' = derivative e in
  let g' = g -. evaluate (Binop (Div, e, e')) g in
  find_zero e g' epsilon (lim - 1)
;;



(*>* Problem 2.5 *>*)

(* See writeup for instructions. *)
let rec find_zero_exact (e:expression) : expression option =
  let rec find_d1expr (e:expression): (float * float) =
    match e with
    | Num n -> (0., n)
    | Var -> (1., 0.)
    | Binop (b, e1, e2) -> (
      let (a1, b1) = find_d1expr e1 in
      let (a2, b2) = find_d1expr e2 in
      match b with
      | Add -> (a1 +. a2, b1 +. b2)
      | Sub -> (a1 -. a2, b1 -. b2)
      | Mul -> if (a1 <> 0.) && (a2 <> 0.) then invalid_arg "no deg 1"
  else (a1 *. a2 +. a1 *. b2 +. a2 *. b1, b1 *. b2)
      | _ -> invalid_arg "no deg 1")
    | Unop (u, e1) -> (
      let (a, b) = find_d1expr e1 in
      match u with
      | Neg -> (0. -. a, 0. -. b)
      | _ -> invalid_arg "no deg 1") in
  let a, b = find_d1expr e in
  Some (Binop (Div, Num (0. -. b), Num a))
;;



