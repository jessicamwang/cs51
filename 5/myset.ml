open Core.Std

(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Ordering.t
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
  open Order
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Equal -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right xs ~f:insert ~init:ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Equal -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left ~f:(fun a x -> f x a) ~init:e

  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left s ~f:f ~init:"") ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left lst ~f:(fun r k -> insert k r) ~init:d

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter elts ~f:(fun k -> assert(member s1 k)) ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right elts ~f:(fun k r -> remove k r) ~init:s1 in
    List.iter elts ~f:(fun k -> assert(not (member s2 k))) ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(struct
      open Order
      type key = C.t
      type value = C.t
      let compare = C.compare
      let string_of_key = C.string_of_t
      let string_of_value = C.string_of_t
      let gen_key = C.gen 
      let gen_key_gt = C.gen_gt 
      let gen_key_lt = C.gen_lt
      let gen_key_between = C.gen_between 
      let gen_key_random = C.gen_random
      let gen_value = C.gen_random
      let gen_pair () = (gen_key(),gen_value())
  end)

  type elt = D.key
  type set = D.dict
  let empty = D.empty
  let insert k d = (D.insert d k k)
  let singleton k = insert k empty
  let member = D.member
  let remove k d = (D.remove d k)
  let choose d = 
      match (D.choose d) with
      |None -> None 
      |Some (k, _, d1) -> Some (k, d1)
  let is_empty d =
      match (choose d) with 
      |None -> true
      |Some (_, _) -> false
  let fold f = D.fold (fun k _ a -> f k a)
  let union = fold insert
  let intersect s1 s2 =
      (fold (fun e s -> if member s2 e then s else remove e s) s1 s2)

  (* implement the rest of the functions in the signature! *)

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* add your test functions to run_tests *)
  
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left lst ~f:(fun r k -> insert k r) ~init:d

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))
    
  let test_member () =
    let elts = generate_random_list 100 in
    List.iter  elts ~f:(fun k -> assert(not (member empty k)));
    let s1 = insert_list empty elts in
    List.iter elts ~f:(fun k -> assert (member s1 k));
    ()

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter ~f:(fun k -> assert(member s1 k)) elts;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right elts ~f:(fun k r -> remove k r) ~init:s1 in
    List.iter elts ~f:(fun k -> assert(not (member s2 k))) ;
    ()
    
  let set_equal (s1: set) (s2:set) : bool =
    (fold (fun k a -> (member s1 k) && a) true s2) && (fold (fun k a -> (member s2 k) && a) true s1)
    
  let test_union () = 
    let elts1 = generate_random_list 100 in
    let s1 = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2 = insert_list empty elts2 in
    let unions1s2= union s1 s2 in
    assert(set_equal (union empty empty) =empty);
    List.iter elts1 ~f:(fun x -> Printf.printf "%s\n" (string_of_elt x));
    Printf.printf "%s\n" (string_of_set (union empty s1));
    Printf.printf "%s\n" (string_of_set (s1));
    assert(set_equal (union empty s1) s1);
    List.iter elts1 ~f:(fun k -> assert(member unions1s2 k));
    List.iter elts2 ~f:(fun k -> assert(member unions1s2 k));
    ()
  
  let test_intersect () =
    let elts1 = generate_random_list 100 in
    let s1 = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2 = insert_list empty elts2 in
    let intersects1s1 = intersect s1 s1 in
    List.iter elts1 ~f:(fun k -> assert(member intersects1s1 k));
    let intersects1empty = intersect s1 empty in
    List.iter elts1 ~f:(fun k -> assert(not (member intersects1empty k)));
    (*let intersects1s2 = intersect s1 s2 in
    let eltsintersect = list_intersect elts1 elts 2 in
    List.iter eltsintersect ~f:(fun k -> assert(member intersects1s2 k));*)
    ()
    
  let rec choose_until_empty (size : int) (s:set) : int =
    match choose s with
    |None -> size
    |Some(_,s1) -> choose_until_empty (size+1) s1
    
  let test_choose () =
    let elts1 = generate_random_list 100 in
    let s1 = insert_list empty elts1 in
    List.iter elts1 ~f:(fun _ -> assert(not ((choose s1) = None )));
    assert((choose_until_empty 0 s1) = 100);
    assert((choose empty) = None);
    ()
    
  let test_fold () =
    let elts1 = generate_random_list 100 in
    let s1 = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2 = insert_list empty elts2 in
    let intersects1s2 = intersect s1 s2 in 
    assert( fold (fun k a -> (List.exists ~f:(fun x -> x=k) elts1) && a) true s1);
    assert( not (fold (fun k a -> (List.exists ~f:(fun x -> x=k) elts1) && a) false s1));
    assert( fold (fun k a -> (List.exists ~f:(fun x -> x=k) elts1) && (List.exists ~f:(fun x -> x=k) elts2) && a ) true intersects1s2);
    ()
    
  let test_is_empty () =
    let elts1 = generate_random_list 100 in
    let s1 = insert_list empty elts1 in
    assert( not(is_empty s1));
    assert(is_empty empty);
    ()
    
  let test_singleton () =
    let elt = C.gen_random() in
    let s1 = insert elt empty in
    let s2 = singleton (elt) in
    assert (s1 = s2);
    ()
  
  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()
end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
(*
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;
*)

(* Create a set of ints using our DictSet functor
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)
module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;


(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use our dictionary implementation when your are
   * finished. *)
  ListSet (C)
 (* DictSet (C)*)

