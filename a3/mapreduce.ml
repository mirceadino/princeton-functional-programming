(*

Name:
  Email:
    Minutes Spent on Problem 1.1:
      Minutes Spent on Problem 1.2:

        (You aren't in any way graded on the number of minutes spent;
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:

  *)

(* This part of the assignment uses the following functions
 * If you forget how they work, look them up:
   * http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *)

let map : ('a -> 'b) -> 'a list -> 'b list = List.map;;

let filter : ('a -> bool) -> 'a list -> 'a list = List.filter;;

let foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = List.fold_right;;

let foldl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b = List.fold_left;;

(* reduce is equivalent to List.fold_right,
 * only its args are ordered differently *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl) ;;

(***********************************************)
(******            1.1 WARM UP            ******)
(***********************************************)

(* Solve each problem in this part using map, reduce, foldl, foldr or filter.
 * Map, filter, reduce, foldl, foldr are an example of a *combinator library* --
 * a library of higher-order functions used to solve problems in a particular
 * domain.  In this case, the domain is list-processing.  However, there are
 * countless other useful combinator libraries.  The goal is to get used to
 * thinking about how to decompose complex functions in to smaller, simpler,
 * orthogonal functional components.  The smaller, simpler functional
 * components can be constructed directly using the combinator library.
 *
 * Note: foldl is slightly more efficient than foldr because it is tail
 * recursive.  (We will explain what that means later in the course.)
 * Hence, solutions that use foldl are typically superior to solutions
 * that use foldr, all other things being equal.  For extra Karma, use foldl
 * where you can instead of foldr (but take care to retain good style --
 * a horribly convoluted, incomprehensible function that uses foldl is worse
 * than an elegant one that uses foldr).
 *
 * In these problems, you are not allowed to use the "rec" keyword in
* your solution.  A solution, even a working one, that uses explicit
 * recursion via "rec" will receive little to no credit.  You may write
 * useful auxiliary functions; they just may not be recursive.
 *
 * You are also not allowed to use other functions from the list library
 * such as sort, concat or flatten.  (You are allowed to recode those
 * functions yourself using map, filter, fold if you find it necessary.)
 *
 *)

(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
  let f (a: int): int = -a in
  map f nums
;;

(* Unit test example.  Uncomment after writing negate_all *)
assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);;


(*>* Problem 1.1.b *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)
let sum_rows (rows:int list list) : int list =
  let s (a: int) (b:int): int = a + b in
  let r (a: int list): int = foldl s 0 a in
  let f (a: int list): int = r a in
  map f rows
;;

assert ((sum_rows [[1;2]; [3;4]]) = [3;7]);;


(*>* Problem 1.1.c *>*)

(*  limit_range : Returns a list of numbers in the input list within a
 *                  given range (inclusive), in the same order they appeared
 *                  in the input list.
 *       Example : limit_range [1;3;4;5;2] (1,3) = [1;3;2] *)
let limit_range (nums:int list) (range:int * int) : int list =
  let lo, hi = range in
  let m (a: int): int list =
    if (lo <= a) && (a <= hi) then [a]
    else [] in
  let s (a: int list) (b: int list): int list =
    match a, b with
    | [], _ -> b
    | [x], _ -> x::b
    | _, _ -> [] in
  foldr s (map m nums) []
;;

assert ((limit_range [1;3;4;5;2] (1,3)) = [1;3;2]);;


(*>* Problem 1.1.d *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)
let num_occurs (n:int) (nums:int list) : int =
  let m (a: int): int = if a = n then 1 else 0 in
  let s (a: int) (b: int): int = a + b in
  foldl s 0 (map m nums);;

assert ((num_occurs 4 [1;3;4;5;4]) = 2);;


(*>* Problem 1.1.e *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)
let super_sum (nlists:int list list) : int =
  let s (a: int) (b: int): int = a + b in
  let m (a: int list): int = foldl s 0 a in
  foldl s 0 (map m nlists)
;;

assert ((super_sum [[1;2;3];[];[5]]) = 11);;


(****************************************************)
(**********       1.2 A Bigger Challenge   **********)
(****************************************************)

(*
 * Note: some of these questions may be challenging.
 * Don't neglect Part 2 of this assignment because you are working on
 * these problems.
 *)

(*>* Problem 1.2.a *>*)

(* min2: returns the second-smallest element of a list, when put into
 * sorted order. Note that if list contains duplicates, the second-smallest
 * element and the smallest element may be identical; your code should return
 * it.
 *
 * Example: min2 [2110; 4820; 3110; 4120] = 3110.
 * Example: min2 [2110; 4820; 2110; 4120] = 2110.
 *
 * For full credit, use a fold function, do not sort the list and do not
 * use the rec keyword (aside from using folds).
 *
 * You will receive partial credit if you use explicit recursion instead of
 * a fold.
 *
 * If the list contains 0 or 1 elements, call (invalid_arg s) with a helpful
 * string s. See the Pervasives library for the invalid_arg function and
 * other useful exception-raising functions:
   *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
 *)

let min2 (xs:int list) : int =
  match xs with
  | [] -> invalid_arg "mlc"
  | [x] -> invalid_arg "mlc"
  | _ -> let twomins (a,b: int*int) (c: int): (int*int) =
    if c > b then (a, b)
    else if c > a then (a, c)
    else (c, a) in
  let infinity = 10000000 in
  let a, b = foldl twomins (infinity, infinity) xs in b
;;

assert ((min2 [2110; 4820; 3110; 4120]) = 3110);;
assert ((min2 [2110; 4820; 2110; 4180]) = 2110);;

(*>* Problem 1.2.b *>*)

(* consec_dedupe : removes consecutive duplicate values from a list.
 * More specifically, consec_dedupe has two arguments:
   *  eq is a function equiv representing an equivalence relation
 *  xs is a list of elements.
 * It returns a list containing the same elements as lst, but without
 * any duplicates, where two elements are considered equal if applying eq
 * to them yields true.
 *
 * Example: consec_dedupe (=) [1; 1; 1; 3; 4; 4; 3] = [1; 3; 4; 3].
 *
 * Example:
   *
 * let nocase_eq (s1:string) (s2:string) : bool =
   *   (String.uppercase s1) = (String.uppercase s2)
 * ;;
*
 * consec_dedupe nocase_eq ["hi"; "HI"; "bi"] = ["hi"; "bi"]
 *
 * (When consecutive duplicates are not exactly syntactically equal
 * as above, it does not matter which of the duplicates are discarded.)
 *
 * Again, for full credit, do not use explicit recursion (the rec keyword),
 * but instead use foldr or foldl (or both).
 *
 * Partial credit will be given to solutions that do use explicit recursion.
 *)

let consec_dedupe (eq:'a -> 'a -> bool) (xs:'a list) : 'a list =
  failwith "Not implemented"
;;

(*>* Problem 1.2.c *>*)

(* prefixes: return a list of all non-empty prefixes of a list,
 * ordered from shortest to longest.

    Example: prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]].
    *)

let prefixes (xs: 'a list) : 'a list list =
  let f (a: 'a list list) (b: 'a): 'a list list =
    match a with
    | [[]] -> [[b]]
    | [c] -> c::[c::b]
    | [c::rest] -> rest
  in
    foldl f xs [[]]
;;

(*>* Problem 1.2.d *>*)

(* k_sublist : Given a list of integers nums and an integer k,
 * the function k_sublist computes the contiguous sublist of length k
 * whose elements have the largest sum.

    Example: k_sublist [1; 2; 4; 5; 8] 3 = [4; 5; 8].
    *)

let k_sublist (nums: int list) (k:int) : int list =
  failwith "Not implemented"
;;


