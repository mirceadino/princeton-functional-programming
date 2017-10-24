(* Box office analysis *)

(* Contents:
  -- the movie type
    -- the studio_gross type
    -- functions for querying and transforming lists of movies
    *)

(* a movie is a tuple of (title, studio, gross in millions, year) *)
type movie = string * string * float * int;;

(* a studio_gross is a pair of (studio, gross in millions) *)
type studio_gross = string * float;;

(* call bad_argument if your function receives a bad argument *)
(* do not change this exception or function                   *)
exception Bad_arg of string
let bad_arg (s:string) = raise (Bad_arg s)

(* a useful debugging routine *)
let debug s = print_string s; flush_all()

(* *** DO NOT CHANGE DEFINITIONS ABOVE THIS LINE! *** *)

let rec sum (movies : movie list) : float =
  match movies with
  | [] -> 0.0
  | (title, studio, gross, year) :: movies' -> gross +. sum movies'
;;

let rec length (movies : movie list) : int =
  match movies with
  | [] -> 0
  | m :: movies' -> 1 + length movies'
;;

(* find the average gross of the movies in the list                  *)
(* hint: you may need to use functions float_of_int and int_of_float *)
(* hint: if you don't know what those functions do,                  *)
(*       type them in to ocaml toplevel                              *)
(* hint: recall the difference between +. and + also 0. and 0        *)
let average (movies : movie list) : float =
  sum movies /. float_of_int (length movies)
;;

(* return a list containing only the movies from the given decade *)
(* call bad_arg if n is not 20, 30, ..., 90, 00, 10               *)
let rec decade (n:int) (movies:movie list) : movie list =
  if n mod 10 != 0 || n < 0 || n >= 100 then
    bad_arg "n is not 20, 30, ..., 90, 00, 10";
  match movies with
  | [] -> []
  | m :: movies' ->
      let (title, studio, gross, year) = m in
      if (year mod 100) / 10 = n / 10 then
        m :: decade n movies'
      else
        decade n movies'
;;

(* return the first n items from the list *)
let rec take (n:int) (l:'a list)  : 'a list =
  match n with
  | 0 -> []
  | _ ->
      match l with
      | [] -> []
      | x :: l' -> x :: take (n-1) l'
;;

(* return everything but the first n items from the list *)
let rec drop (n:int) (l:'a list)  : 'a list =
  match n with
  | 0 -> l
  | _ ->
      match l with
      | [] -> []
      | a :: l' -> drop (n-1) l'
;;

(* return a list [x1; x2; ...; xn] with the same elements as the input l
   and where:
     leq xn xn-1
     ...
     leq x3 x2
     leq x2 x1
     are all true
     *)
(* hint: define an auxiliary function "select" *)
(* hint: select probably should return a pair.  Of what?  Think functionally. *)
type 'a less = 'a -> 'a -> bool;;

let rec remove_element (e: 'a) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: l' ->
      if x = e then l'
      else x :: remove_element e l'
;;

let rec all_true (leq:'a less) (x:'a) (l:'a list): bool =
  match l with
  | [] -> true
  | a :: l' ->
      let eq a b = not (leq a b || leq b a) in
      if leq x a || eq x a then all_true leq x l'
      else false
;;

let rec select_min (leq:'a less) (l:'a list) (r:'a list): 'a * ('a list) =
  match r with
  | [] -> bad_arg "this shouldn't happen"
  | [x] -> (x, remove_element x l)
  | x :: r' ->
      let p = remove_element x l in
      if all_true leq x p then (x, p)
      else select_min leq l r'
;;

let rec selection_sort (leq:'a less) (l:'a list) : 'a list =
  match l with
  | [] -> []
  | l ->
      let (x, r) = select_min leq l l in
      x :: selection_sort leq r
;;

(* return list of movies sorted by gross (largest gross first) *)
let sort_by_gross (movies : movie list) : movie list =
  let leq (a: movie) (b: movie): bool =
    let (t0, s0, gross0, y0) = a in
    let (t1, s1, gross1, y1) = b in
    gross0 > gross1 in
  selection_sort leq movies
;;

(* return list of movies sorted by year produced (largest year first) *)
let sort_by_year (movies : movie list) : movie list =
  let leq (a: movie) (b: movie): bool =
    let (t0, s0, g0, year0) = a in
    let (t1, s1, g1, year1) = b in
    year0 > year1 in
  selection_sort leq movies
;;

(* sort list of (studio, gross in millions) by gross in millions
 * with the largest gross first *)
let sort_by_studio (studio_grosses : studio_gross list) : studio_gross list =
  let leq (a: studio_gross) (b: studio_gross): bool =
    let (s0, gross0) = a in
    let (s1, gross1) = b in
    gross0 > gross1 in
  selection_sort leq studio_grosses
;;

(* given list of movies,
 * return list of pairs (studio_name, total gross revenue for that studio)  *)
let rec by_studio (movies:movie list) : studio_gross list =
  let rec add_movie_to_studio movie studios =
    match studios with
    | [] ->
        let (t,s,g,y) = movie in
        [(s, g)]
    | s :: studios' ->
        let (t, studio,g,y) = movie in
        let (studio', gross) = s in
        if studio = studio' then (studio, gross +. g) :: studios'
        else s :: add_movie_to_studio movie studios'
        in
  match movies with
  | [] -> []
  | movie :: movies' ->
      let studio_grosses = by_studio movies' in
      add_movie_to_studio movie studio_grosses
;;

(*********************************)
(* Karma -- Optional Questions!! *)
(*********************************)

(* return the list of all permutations of input list *)
(* test this on small inputs!! *)
let perm (items:'a list) : 'a list list =
  failwith "perm unimplemented"
;;

(* randomize the order of the items in the list *)
(* explain your method in a comment *)
(* extra karma: do it in time less than N^2 where N is the length of the list *)
let scramble (items:'a list) : 'a list =
  failwith "scramble unimplemented"
;;

(***********)
(* Testing *)
(***********)

(* Augment the testing infrastructure below as you see fit *)

(* Test Data *)

let data1 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003)
  ];;

let data2 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012)
];;

let data3 : movie list = [
  ("Harry Potter and the Sorcerer's Stone","WB",317.57555,2001);
  ("Star Wars: Episode II - Attack of the Clones","Fox",310.67674,2002);
  ("Return of the Jedi", "Fox", 309.306177, 1983)
];;

let data4 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012);
  ("The Dark Knight","WB",533.34,2008);
  ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011)
];;

(* Assertion Testing *)

assert(take 0 data4 = []);;
assert(take 1 data1 = data1);;
assert(take 2 data4 = data2);;
assert(take 5 data2 = data2);;
assert(take 2 data2 = data2);;

(* Additional Testing Infrastructure *)

let stests : (unit -> movie list) list = [
  (fun () -> sort_by_gross data1);
  (fun () -> sort_by_gross data2);
  (fun () -> sort_by_gross data3);
  (fun () -> sort_by_gross data4)
];;

let check (i:int) (tests:(unit -> 'a) list) : 'a =
  if i < List.length tests && i >= 0 then
    List.nth tests i ()
        else
          failwith ("bad test" ^ string_of_int i)
;;

