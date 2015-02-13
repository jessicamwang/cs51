(*** CS 51 Problem Set 1 ***)
(*** February 6, 2015 ***)
(*** Charles Liu ***)

open Core.Std


(* Problem 1 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

(*>* Problem 1a *>*)
let prob1a : bytes = let greet y = "Hello " ^ y in greet "World!";;

(*>* Problem 1b *>*)
let prob1b : int option list = [Some 7; Some 1; None; Some 1];;

(*>* Problem 1c *>*)
let prob1c : ('a option * float option) * bool = ((None, Some 42.0), true);;


(* Explain in a comment why the following will not type check,
   and provide a fix *)

(*>* Problem 1d *>*)
(*
let prob1d : string * int list = [("CS", 51); ("CS", 50)];;
*)
(* This will not type check because the type is read as a pair with a string
 * and a list of ints, when it really should be a list of pairs, each with a
 * string and int. *)
(* Problem 1d fix *)
let prob1d : (string * int) list = [("CS", 51); ("CS", 50)];;

(*>* Problem 1e *>*)
(*
let prob1e : int =
  let compare (x,y) = x < y in
  if compare (5, 4.9) then 5 else 2;;
*)
(* This will not type check because compare expects x and y to be of the same
 * type, and when compare (5, 4.9) is called it is given an int and a float,
 * which are different types. *)
(* Problem 1e fix *)
let prob1e : int =
  let compare (x,y) = x < y in
  if compare (5., 4.9) then 5 else 2;;

(*>* Problem 1f *>*)
(*
let prob1f : (string * string) list =
  [("January", None); ("February", None); ("March", 15); ("April", None);
   ("May", None); ("June", 1); ("July", 4); ("August", None);
   ("September", 3); ("October", 1); ("November", 2); ("December", 11)] ;;
*)
(* This will not type check because the second argument of each pair is 
 * an int option, not a string. Also, the integers by themselves are a
 * different type from None, so they must be changed to int options by
 * using the Some constructor. *)
(* Problem 1f fix *)
let prob1f : (string * int option) list =
  [("January", None); ("February", None); ("March", Some 15); ("April", None);
   ("May", None); ("June", Some 1); ("July", Some 4); ("August", None);
   ("September", Some 3); ("October", Some 1); ("November", Some 2);
   ("December", Some 11)] ;;



(* Problem 2 - Write the following functions *)

(*>* Problem 2a *>*)

(* reversed: takes a list of integers and returns true if the integers in the
 * list are in decreasing order. The empty list is considered to be reversed.
 * Consecutive elements can be equal in a reversed list. *)
let rec reversed (lst : int list) : bool =
  match lst with
  | [] -> true
  | hd :: tl -> 
    (match tl with
     | [] -> true
     | tl_value :: _ -> hd >= tl_value && reversed tl)
;;

(* Problem 2a unit tests *)

let () = assert ((reversed []) = true);;
let () = assert ((reversed [10]) = true);;
let () = assert ((reversed [1;5;3]) = false);;
let () = assert ((reversed [7;6;5;4;3;2;1]) = true);;
let () = assert ((reversed [7;7;5;5;5;2;1]) = true);;
let () = assert ((reversed [5;5;7;5;5;2;1]) = false);;
let () = assert ((reversed [7;6;5;4;3;1;2]) = false);;
let () = assert ((reversed [5;-1;-9;-100]) = true);;
let () = assert ((reversed [10;-5;0;-100]) = false);;


(*>* Problem 2b *>*)

(* merge: takes two integer lists, each sorted in increasing order,
 and returns a single merged list in sorted order *)
let rec merge (lst1 : int list) (lst2 : int list) : int list =
  match (lst1,lst2) with
  | ([],_) -> lst2
  | (_,[]) -> lst1
  | (hd1 :: tl1, hd2 :: tl2) ->
    if hd1 <= hd2 then hd1 :: merge tl1 lst2
    else hd2 :: merge lst1 tl2
;;

(* Problem 2b unit tests *)
let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [2;2;4;4] [1;2;3]) = [1;2;2;2;3;4;4]);;
let () = assert ((merge [1;3] [1;3]) = [1;1;3;3]);;
let () = assert ((merge [-1;2;3;42] [-1;6;1001]) = [-1;-1;2;3;6;42;1001]);;
let () = assert ((merge [1] [1]) = [1;1]);;
let () = assert ((merge [1;4;5;7] [2;3;6]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [1] []) = [1]);;
let () = assert ((merge [] [-7]) = [-7]);;
let () = assert ((merge [1] [-1]) = [-1;1]);;


(*>* Problem 2c *>*)

(* unzip: takes a list of pairs and returns a pair of lists, the
 * first of which contains each first element of each pair, and
 * the second of which contains each second element (all elements
 * in the same order as in the input). *)
let rec unzip (pair_lst : (int * int) list) : int list * int list =
  match pair_lst with
  | [] -> ([],[])
  | (fst,snd) :: tl -> 
    let (lst1, lst2) = unzip tl in
    (fst :: lst1, snd :: lst2)
;;

(* Problem 2c unit tests *)
let () = assert ((unzip []) = ([],[]));;
let () = assert ((unzip [(1,5)]) = ([1],[5]));;
let () = assert ((unzip [(1,2);(3,4);(5,6)]) = ([1;3;5],[2;4;6]));;
let () = assert ((unzip [(7,1);(-1,4);(3,2)]) = ([7;-1;3],[1;4;2]));;
let () = assert ((unzip [(3,1);(-1,4);(3,1)]) = ([3;-1;3],[1;4;1]));;
let () = assert ((unzip [(7,1);(3,1);(-1,4);(3,1)]) 
                 = ([7;3;-1;3],[1;1;4;1]));;
let () = assert ((unzip [(1,1);(-1,4);(3,2);(7,0);(8,0);(10,10)]) 
                 = ([1;-1;3;7;8;10],[1;4;2;0;0;10]));;


(*>* Problem 2d *>*)

(* len: returns the length of a list of generics *)
let rec len (lst : 'a list) : int =
  match lst with
  | [] -> 0
  | _ :: tl -> 1 + len tl
;;

(* sum: returns the sum of a list of floats *)
let rec sum (lst : float list) : float =
  match lst with
  | [] -> 0.
  | hd :: tl -> hd +. sum tl
;;

(* arith_mean: returns the arithmetic mean of a list of floats. *)
let arith_mean (lst : float list) : float =
  sum lst /. float (len lst)
;;

(* square: returns the square of a float. *)
let square (x : float) : float = x *. x;;

(* variance: returns the variance of a list of floats (returns None if 
 * fewer than 2 floats) as a float option. The variance is given by
 * 1/(n-1) * sum (x_i-m)^2, where m is the arithmetic mean of the list. *)
let variance (lst : float list) : float option =
  match lst with
  | [] -> None
  | [_] -> None
  | lst -> 
    let mean = arith_mean lst in
    let rec sum_squared_error (lst' :float list) : float = 
      match lst' with
      | [] -> 0.
      | hd :: tl -> square (hd -. mean) +. sum_squared_error tl
    in
    Some ((sum_squared_error lst) /. (float (len lst - 1)))
;;


(* float_opt_equal: checks if the values of float options are equal 
 * or if both None. *)
let float_opt_equal (x : float option) (y : float option) : bool =
  match (x,y) with
  | (None,None) -> true
  | (None,_) -> false
  | (_,None) -> false
  | (Some a,Some b) -> (a -. b < 0.00001) && (b -. a < 0.00001)
;;

(* Problem 2d unit tests *)
let () = assert (float_opt_equal (variance []) None);;
let () = assert (float_opt_equal (variance [1.0]) None);;
let () = assert (float_opt_equal (variance [1.0; 2.0]) (Some 0.5));;
let () = assert (float_opt_equal (variance [1.0; 2.0; 3.0; 4.0; 5.0]) 
                                 (Some 2.5));;
let () = assert (float_opt_equal (variance [1.0; 1.0; 1.0]) (Some 0.));;
let () = assert (float_opt_equal (variance [-3.0; -2.0; -4.0; -1.0; -5.0]) 
                                 (Some 2.5));;
let () = assert (float_opt_equal (variance [1.0; 7.0; 3.0; 3.5; 6.8]) 
                                 (Some 6.688));;


(*>* Problem 2e *>*)

(* few_divisors n m: returns true if n has fewer than m divisors,
 * (including 1 and n) and false otherwise. *)
let few_divisors (n : int) (m : int) : bool =
  let rec divisor_count divisor count =
    match divisor with
    | 0 -> count
    | _ -> 
      if n % divisor = 0 then divisor_count (divisor - 1) (count + 1)
      else divisor_count (divisor - 1) count
  in
  divisor_count n 0 < m
;;

(* Problem 2e unit tests *)
let () = assert ((few_divisors 1 1) = false);;
let () = assert ((few_divisors 1 2) = true);;
let () = assert ((few_divisors 2 2) = false);;
let () = assert ((few_divisors 2 3) = true);;
let () = assert ((few_divisors 6 4) = false);;
let () = assert ((few_divisors 6 5) = true);;
let () = assert ((few_divisors 10 3) = false);;
let () = assert ((few_divisors 10 10) = true);;

(*>* Problem 2f *>*)

(* concat_list: returns one string with all the string elements of
 * lst concatenated together separated by the string sep. *)

let rec concat_list (sep : string) (lst : string list) : string = 
  match lst with
  | [] -> ""
  | [str] -> str
  | hd :: tl -> hd ^ sep ^ concat_list sep tl
;;

(* Problem 2f unit tests *)
let () = assert ((concat_list "..." []) = "");;
let () = assert ((concat_list "..." ["Hi"]) = "Hi");;
let () = assert ((concat_list ", " ["Hello";"World!"]) = "Hello, World!");;
let () = assert ((concat_list "*" ["1";"2";"3"]) = "1*2*3");;
let () = assert ((concat_list "." ["^";"^";"O";"o"]) = "^.^.O.o");;
let () = assert ((concat_list "..." ["Old";"Man";"Sea"]) = "Old...Man...Sea");;

(*>* Problem 2g *>*)

(* to_run_length: generates the run-length encoding of a string *)
let rec to_run_length (lst : char list) : (int * char) list = 
  match lst with
  | [] -> []
  | hd :: tl -> 
    (match tl with
     | [] -> [(1,hd)]
     | next :: _ -> 
       if hd = next then 
         match to_run_length tl with
         | [] -> []
         | (count,ch) :: run_tl ->
           (1+count,ch) :: run_tl         
       else (1,hd) :: to_run_length tl)
;;

(* from_run_length: generates a string from a run-length encoding *)
let rec from_run_length (runs : (int * char) list) : char list =
  match runs with
  | [] -> []
  | (len,ch) :: tl -> 
    if len = 0 then from_run_length tl
    else ch :: from_run_length ((len-1,ch) :: tl)
;;

(* Problem 2g unit tests *)
let () = assert ((to_run_length []) = []);;
let () = assert ((to_run_length ['a']) = [(1,'a')]);;
let () = assert ((to_run_length ['a';'a']) = [(2,'a')]);;
let () = assert ((to_run_length ['a';'b';'c']) = [(1,'a');(1,'b');(1,'c')]);;
let () = assert ((to_run_length ['a';'a';'b';'c';'c';'c'])
                 = [(2,'a');(1,'b');(3,'c')]);;
let () = assert ((to_run_length ['a';'b';'a']) = [(1,'a');(1,'b');(1,'a')]);;
let () = assert ((from_run_length []) = []);;
let () = assert ((from_run_length [(1,'a')]) = ['a']);;
let () = assert ((from_run_length [(2,'a')]) = ['a';'a']);;
let () = assert ((from_run_length [(1,'a');(1,'b');(1,'c')]) = ['a';'b';'c']);;
let () = assert ((from_run_length[(2,'a');(1,'b');(3,'c')] )
                 = ['a';'a';'b';'c';'c';'c']);;
let () = assert ((from_run_length [(1,'a');(1,'b');(1,'a')]) = ['a';'b';'a']);;


(*>* Problem 3 *>*)

(* Challenge!

 * permutations lst should return a list containing every
 * permutation of lst. For example, one correct answer to
 * permutations [1; 2; 3] is
 * [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]].

 * It doesn't matter what order the permutations appear in the returned list.
 * Note that if the input list is of length n then the answer should be of
 * length n!.

 * Hint:
 * One way to do this is to write an auxiliary function,
 * interleave : int -> int list -> int list list,
 * that yields all interleavings of its first argument into its second:
 * interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ].
 * You may also find occasion for the library functions
 * List.map and List.concat. *)

(* The type signature for permuations is: *)
(* permutations : int list -> int list list *)

(* interleave: interleaves x to all possible positions in lst *)
let interleave (x : int) (lst : int list) : int list list =
  let rec inter (bef : int list) (aft : int list) : int list list =
    (bef @ (x :: aft)) :: 
    match aft with
    | [] -> []
    | aft_hd :: aft_tl -> inter (bef @ [aft_hd]) aft_tl
  in
  inter [] lst
;;

(* permutations: generates all permutations of a list of ints *)
let rec permutations (lst : int list) : int list list =
  match lst with
  | [] -> [[]]
  | hd :: tl -> 
    List.concat (List.map (permutations tl) ~f:(fun lst' -> interleave hd lst'))
;;

(* Problem 3 unit tests *)
let () = assert ((permutations []) = [[]]);;
let () = assert ((permutations [1]) = [[1]]);;
let () = assert ((permutations [1;2]) = [[1;2];[2;1]]);;
let () = assert ((permutations [1;2;3]) = [[1;2;3];[2;1;3];[2;3;1];
                                           [1;3;2];[3;1;2];[3;2;1]]);;
let () = assert ((permutations [1;5;7]) = [[1;5;7];[5;1;7];[5;7;1];
                                           [1;7;5];[7;1;5];[7;5;1]]);;
let () = assert ((permutations [1;2;3;4]) = [[1;2;3;4];[2;1;3;4];[2;3;1;4];
                                             [2;3;4;1];[1;3;2;4];[3;1;2;4];
                                             [3;2;1;4];[3;2;4;1];[1;3;4;2];
                                             [3;1;4;2];[3;4;1;2];[3;4;2;1];
                                             [1;2;4;3];[2;1;4;3];[2;4;1;3];
                                             [2;4;3;1];[1;4;2;3];[4;1;2;3];
                                             [4;2;1;3];[4;2;3;1];[1;4;3;2];
                                             [4;1;3;2];[4;3;1;2];[4;3;2;1]]);;
