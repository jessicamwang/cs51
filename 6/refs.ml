(* NAMES:
 *
 * Partner 1's name: Charles Liu
 * Partner 1's code.seas account: charlesliu
 *
 * (Leave blank if you are working alone)
 * Partner 2's name: Jessica Wang
 * Partner 2's code.seas account: jessicawang
 *)

open Core.Std

(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

let length_intro_cycle (lst : 'a mlist) : int * int =
  let rec find_cyc (f : ('a mlist) ref) (s : ('a mlist) ref) (n : int) : 
    ('a mlist) ref * int =
    match !f with 
    | Nil -> (ref Nil, n)
    | Cons(_, fnext) ->
      match !fnext with
      | Nil -> (ref Nil, n+1)
      | Cons(_, fnext') -> 
        match !s with
        | Nil -> failwith "Slow reached end of list before fast"
        | Cons(_, snext) -> 
          if phys_equal !fnext' !snext then (fnext', n+2)
          else find_cyc fnext' snext (n+2)
  in
  let (cyc, ind) = find_cyc (ref lst) (ref lst) 0 in
  match !cyc with
  | Nil -> (ind, 0)
  | _ ->
    let rec find_start (f : ('a mlist) ref) (s : ('a mlist) ref) (n : int) :
      ('a mlist) ref * int =
      if phys_equal !f !s then (f, n)
      else 
        match (!f, !s) with
        | (Cons(_, fnext), Cons(_, snext)) -> find_start fnext snext (n+1)
        | _ -> failwith "No cycle exists"
    in
    let (start, intro_len) = find_start cyc (ref lst) 0 in
    match !start with 
    | Nil -> failwith "No cycle exists"
    | Cons(_, init) ->
      let rec cycle_length (cur : (' a mlist) ref) (n : int) : int =
        if phys_equal cur start then n
        else
          match !cur with 
          | Nil -> failwith "No cycle exists"
          | Cons(_, next) -> cycle_length next (n+1)
      in
      (intro_len, cycle_length init 1)


(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)

let has_cycle (lst : 'a mlist) : bool =
  let (_,cyc_len) = length_intro_cycle lst in
  cyc_len > 0

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)
let rec cycle_list1 = Cons(1, ref cycle_list1)
let rec cycle_list2 = Cons(1, ref (Cons(2, ref cycle_list2)))
let cycle_list3 = Cons(0, ref cycle_list2)

assert(not (has_cycle list1a))
assert(not (has_cycle list1b))
assert(not (has_cycle list1))
assert(has_cycle cycle_list1)
assert(has_cycle cycle_list2)
assert(has_cycle cycle_list3)

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
assert(not (has_cycle list2))
let _ = reflist := list2
assert(has_cycle !reflist)


(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)

let flatten (lst : 'a mlist) : unit =
  let rec advance (l : ('a mlist) ref) (n : int) : ('a mlist) ref =
    if n <= 0 then l
    else
      match !l with
      | Nil -> l
      | Cons(_, next) -> advance next (n-1)
  in
  let (intro_len, cyc_len) = length_intro_cycle lst in
  (advance (ref lst) (intro_len+cyc_len)) := Nil

(*>* Problem 1.3 *>*)
(* Write mlength, which finds the number of nodes in a mutable list. *)

let mlength (lst : 'a mlist) : int =
  let (intro_len, cyc_len) = length_intro_cycle lst in
  intro_len + cyc_len

(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 240
