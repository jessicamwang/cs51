(* CS51 Problem Set 2 *)

open Core.Std

(****************************************************)
(******       1.1: Sparking your INTerest      ******)
(****************************************************)

(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
  List.map nums ~f:(fun (x : int) -> -x)
;;

assert ((negate_all [1; -2; 0]) = [-1; 2; 0]) ;;


(*>* Problem 1.1.b *>*)

(*  sum : Returns the sum of the elements in the list. *)
let sum (nums:int list) : int =
  List.fold_right nums ~f:(fun (x : int) (tot : int) -> x + tot) ~init:0
;;

assert ((sum [-3;4;-7]) = -6);;
assert ((sum []) = 0);;
assert ((sum [1]) = 1);;


(*>* Problem 1.1.c *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input. *)
let sum_rows (rows:int list list) : int list =
  List.map rows ~f:(fun (lst : int list) -> sum lst)
;;

assert ((sum_rows [[7;8;3;5]; [6;4;3]]) = [23;13]);;
assert ((sum_rows [[]; [7;8;5]]) = [0;20]);;


(*>* Problem 1.1.d *>*)

(*  filter_odd : Retains only the odd numbers from the given list. *)
let filter_odd (nums:int list) : int list =
  List.filter nums ~f:(fun (x : int) -> x % 2 = 1)
;;

assert ((filter_odd [1;2;3;5;4;6;8;1]) = [1;3;5;1]);;
assert ((filter_odd [2;4;6]) = []);;


(*>* Problem 1.1.e *>*)

(*  num_occurs : Returns the number of times a given number appears in a
                 list. *)
let num_occurs (n:int) (nums:int list) : int =
  List.fold_right nums ~f:(fun (x : int) (count : int) -> 
                             if x = n then count + 1 else count)
  ~init:0
;;

assert ((num_occurs 4 [1;3;4;5;4]) = 2);;
assert ((num_occurs 0 [1;3;4;5;4]) = 0);;


(*>* Problem 1.1.f *>*)

(*  super_sum : Sums all of the numbers in a list of int lists. *)
let super_sum (nlists:int list list) : int =
    sum (sum_rows nlists)
;;

assert ((super_sum [[1;2;3];[];[5]]) = 11);;

(*>* Problem 1.1.g *>*)

(*  filter_range : Returns a list of numbers in the input list within a
 *                 given range (inclusive), in the same order they appeared
 *                 in the input list. *)
let filter_range (nums:int list) (range:int * int) : int list =
  let (a,b) = range in
  List.filter nums ~f:(fun (x : int) -> x >= a && x <= b)
;;

assert ((filter_range [1;3;4;5;2] (1,3)) = [1;3;2]);;
assert ((filter_range [1;3;4;5;2] (-1,0)) = []);;


(****************************************************)
(**********       1.2 Fun with Types       **********)
(****************************************************)


(*>* Problem 1.2.a *>*)

(*  floats_of_ints : Converts an int list into a list of floats *)
let floats_of_ints (nums:int list) : float list =
  List.map nums ~f:(fun (x : int) -> float x)
;;

assert ((floats_of_ints [1;3;4;6]) = [1.;3.;4.;6.]);;


(*>* Problem 1.2.b *>*)

(*   log10s : Applies the log10 function to all members of a list of floats.
 *            The mathematical function log10 is not defined for
 *            numbers n <= 0, so undefined results should be None. *)
let log10s (lst: float list) : float option list =
  List.map lst ~f:(fun (x : float) -> 
                     if x <= 0. then None else Some (log10 x))
;;

assert ((log10s [1.;0.;10.;-3.;50.]) = [Some 0.; None; Some 1.; None; Some (log10 50.)]);;


(*>* Problem 1.2.c *>*)

(*  deoptionalize : Extracts values from a list of options. *)
let deoptionalize (lst:'a option list) : 'a list =
  List.fold_right lst ~f:(fun (x:'a option) (all:'a list) ->
                            match x with
                            | None -> all
                            | Some a -> a :: all)
                      ~init:[]
;;

assert ((deoptionalize [Some 3; None; Some 5; None; Some 10]) = [3;5;10]);;


(*>* Problem 1.2.d *>*)

(*  some_sum : Sums all of the numbers in a list of int options,
 *             ignoring None values *)
let some_sum (nums:int option list) : int =
  sum (deoptionalize nums)
;;

assert ((some_sum [Some 3; None; Some 5; None; Some 10]) = 18);;
assert ((some_sum [None; None;]) = 0);;


(*>* Problem 1.2.e *>*)

(*  mult_odds : Product of all of the odd members of a list. *)
let mult_odds (nums:int list) : int =
  List.fold_right (filter_odd nums)
  ~f:(fun (x : int) (prod : int) -> x * prod) ~init:1
;;

assert ((mult_odds [1;3;0;2;-5]) = -15);;


(*>* Problem 1.2.f *>*)

(*  concat : Concatenates a list of lists. See the Ocaml library ref *)
let concat (lists:'a list list) : 'a list =
  List.fold_right lists 
  ~f:(fun (lst : 'a list) (all : 'a list) -> lst @ all)
  ~init:[]
;;

assert ((concat [['h';'i'];[' '];['t';'h';'e';'r';'e']]) 
        = ['h';'i';' ';'t';'h';'e';'r';'e']);;


(*>* Problem 1.2.g *>*)

(* the student's name and year *)
type name = string
type year = int
type student = name * year

(*  filter_by_year : returns the names of the students in a given year *)
let filter_by_year (slist:student list) (yr:year) : name list =
  let in_year = List.filter slist ~f:(fun (s : name * year) -> 
                                        let (_,y) = s in
                                        y = yr)
  in
  List.map in_year ~f:(fun (s : name * year) ->
                         let (n,_) = s in n)
;;

assert ((filter_by_year [("Joe",2010);("Bob",2010);("Tom",2013)] 2010)
        = ["Joe";"Bob"]);;

(*>* Problem 1.3 *>*)
let minutes_spent_on_part_1 : int = 90;;
