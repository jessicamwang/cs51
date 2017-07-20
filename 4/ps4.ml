(* PS4
 * Author: Charles Liu
 * Partner: Jessica Wang
 *)

open Core.Std

exception ImplementMe

type order = Equal | Less | Greater

(*****************************************************************************)
(*                              Part 1.5                                     *)
(*****************************************************************************)

(* Please read motivation.ml for some background on why we use functors here *)

(*****************************************************************************)
(*                               Part 2                                      *)
(*****************************************************************************)

(* A better signature for a binary tree, avoiding the comparison function
 * found in motivation.ml. *)
module type BINTREE =
sig
  exception EmptyTree
  exception NodeNotFound

  (* The type of an element in the tree *)
  type elt

  (* What this type actually looks like is left up to the
   * particular BINTREE implementation (i.e. the struct) *)
  type tree

  (* Returns an empty tree *)
  val empty : tree

  (* Search a binary tree for the given value. *)
  val search : elt -> tree -> bool

  (* Insert elt into tree *)
  val insert : elt -> tree -> tree

  (* Delete the given value from a binary tree.
   * May raise NodeNotFound exception. *)
  val delete : elt -> tree -> tree

  (* Return the minimum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmin : tree -> elt

  (* Return the maximum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmax : tree -> elt

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(* A signature for a module which defines a type and
 * how to compare values of that type, as well as ways of generating
 * values of that type. *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> order
  val to_string : t -> string

  (* Generate a value of type t *)
  val generate: unit -> t

  (* Generate a value of type t that is greater than the argument. *)
  val generate_gt: t -> unit -> t

  (* Generate a value of type t that is less than the argument. *)
  val generate_lt: t -> unit -> t

  (* Generate a value of type t that is between argument 1 and argument 2.
   * Returns None if there is no value between argument 1 and argument 2. *)
  val generate_between: t -> t -> unit -> t option
end

(* An example implementation of the COMPARABLE signature. In this
 * example, the value of the integer also gives its priority. *)
module IntCompare : COMPARABLE with type t=int =
struct
  type t = int

  let compare x y = if x < y then Less else if x > y then Greater else Equal

  let to_string = string_of_int

  let generate () = 0

  let generate_gt x () = x + 1

  let generate_lt x () = x - 1

  let generate_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end

(* Another example implementation that only uses the first part
 * of the tuple in comparisons. *)
module IntStringCompare : COMPARABLE with type t=(int * string) =
struct
  type t = int * string
  let compare (p1,_) (p2,_) =
    if p1 < p2 then Less else if p1 > p2 then Greater else Equal

  let to_string (p, s) = "(" ^ string_of_int p ^ "," ^ s ^ ")"


  let () = Random.self_init ()

  let generate () = (0, string_of_int (Random.int Int.max_value))

  let generate_gt (p,s) () = (p+1, s)

  let generate_lt (p,s) () = (p-1, s)

  let generate_between (p1,_) (p2,s2) () =
    let (lower, higher) = (min p1 p2, max p1 p2) in
    (* Reuse the string from the second argument in the output value *)
    if higher - lower < 2 then None else Some (higher - 1, s2)
end

(* BinSTree: takes a module which implements the COMPARABLE signature.
 * Returns a binary search tree module which matches the BINTREE signature.
 * This implementation keeps a list with each node that contains each instance
 * of the value inserted into the tree. *)
module BinSTree(C : COMPARABLE) : BINTREE with type elt = C.t =
struct

  exception EmptyTree
  exception NodeNotFound

  type elt = C.t

  (* One possible type for a tree *)
  type tree = Leaf | Branch of tree * elt list * tree

  (* Representation of the empty tree *)
  let empty = Leaf


(*>* Problem 2.0 *>*)

  (* insert: inserts element x into the tree t, ensuring that all nodes to the
   * left of a given node have smaller values than that node and all nodes to
   * right of a given node have greater values than that node. The most 
   * recently inserted elements are at the front of the list. *)
  let rec insert (x : elt) (t : tree) : tree = 
    match t with
    | Leaf -> Branch (empty, [x], empty)
    | Branch (l, lst, r) ->
      match lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd::_ ->
        match C.compare x hd with
        | Equal -> Branch (l, x::lst, r)
        | Less -> Branch (insert x l, lst, r)
        | Greater -> Branch (l, lst, insert x r)

(*>* Problem 2.1 *>*)

  (* search: returns true if the element x is in tree t, else false *)
  let rec search (x : elt) (t : tree) : bool = 
    match t with
    | Leaf -> false
    | Branch (l, lst, r) ->
      match lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd::_ ->
        match C.compare x hd with
        | Equal -> List.exists ~f:(fun y -> y = x) lst
        | Less -> search x l
        | Greater -> search x r

  (* pull_min: removes the node with minimum value from a binary search tree,
   * returning that node and the new tree. *)
  let rec pull_min (t : tree) : elt list * tree =
    match t with
    | Leaf -> raise EmptyTree
    | Branch (Leaf, v, r) -> (v, r)
    | Branch (l, v, r) -> let min, t' = pull_min l in (min, Branch (t', v, r))


  (* delete: removes an element from the tree. If multiple elements are in the
   * list, removes the one that was inserted first. *)
  let rec delete (x : elt) (t : tree) : tree =
    match t with
    | Leaf -> raise NodeNotFound
    | Branch (l, lst, r) ->
      (* Reverse the list so that we pop off the last element in the list *)
      match List.rev lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd::tl ->
        match C.compare x hd with
        | Less -> Branch (delete x l, lst, r)
        | Greater -> Branch (l, lst, delete x r)
        | Equal ->
          match tl with
          | _::_ -> Branch (l, List.rev tl, r)
          (* The list in the node is empty, so we have to
           * remove the node from the tree.  *)
          | [] ->
            match l, r with
            | Leaf, _ -> r
            | _, Leaf -> l
            | _ -> let v, r' = pull_min r in Branch (l,v,r')

(*>* Problem 2.2 *>*)

  (* getmin: returns the minimum value in a binary search tree. If multiple
   * minimum values exists, returns the one that was inserted first *)
  let getmin (t : tree) : elt = 
    let (lst, _) = pull_min t in
    match List.rev lst with 
    | [] -> failwith "Invalid tree: empty list as node"
    | hd::_ -> hd
  
(*>* Problem 2.3 *>*)

  (* getmax: returns the maximum value of the tree t. Similarly should
   * return the last element in the matching list. *)
  let rec getmax (t : tree) : elt = 
    match t with 
    | Leaf -> raise EmptyTree
    | Branch (_, v, Leaf) -> 
       (match List.rev v with 
        | [] -> failwith "Invalid tree: empty list as node"
        | hd::_ -> hd)
    | Branch (_, _, r) -> getmax r

  (* Inserts elements and checks to make sure the tree structure is correct *)
  let test_insert () =
    let x = C.generate () in
    let t = insert x empty in
    assert (t = Branch (Leaf, [x], Leaf));
    let t = insert x t in
    assert (t = Branch (Leaf, [x;x], Leaf));
    let y = C.generate_gt x () in
    let t = insert y t in
    assert (t = Branch (Leaf, [x;x], Branch(Leaf, [y], Leaf)));
    let z = C.generate_lt x () in
    let t = insert z t in
    assert (t = Branch (Branch(Leaf, [z], Leaf),[x;x],
                Branch (Leaf, [y], Leaf)));
    ()

  (* Insert a bunch of elements, and test to make sure that we
   * can search for all of them. *)
  let test_search () =
    let x = C.generate () in
    let t = insert x empty in
    assert (search x t);
    let order = [ true; false; true; true; true; false; false] in
    let full_tree, values_inserted =
      List.fold_right
        ~f:(fun current_order (tree_so_far, values_so_far) ->
          let prev_value =
            match values_so_far with
            | [] -> x
            | hd :: _ -> hd
          in
          let value =
            if current_order
            then C.generate_gt prev_value ()
            else C.generate_lt prev_value ()
          in
          insert value tree_so_far, value :: values_so_far
        ) ~init:(t, []) order
    in
    List.iter ~f:(fun value -> assert (search value full_tree)) values_inserted

  let test_getmax () =
    let x = C.generate () in
    let x2 = C.generate_lt x () in
    let x3 = C.generate_lt x2 () in
    let x4 = C.generate_lt x3 () in
    assert (getmax (insert x4 (insert x3 (insert x2 (insert x empty)))) = x)

  let test_getmin () =
    let x = C.generate () in
    let x2 = C.generate_gt x () in
    let x3 = C.generate_gt x2 () in
    let x4 = C.generate_gt x3 () in
    assert (getmin (insert x2 (insert x4 (insert x (insert x3 empty)))) = x)

  let test_delete () =
    let x = C.generate () in
    let x2 = C.generate_lt x () in
    let x3 = C.generate_lt x2 () in
    let x4 = C.generate_lt x3 () in
    let after_ins = insert x4 (insert x3 (insert x2 (insert x empty))) in
    assert (delete x (delete x4 (delete x3 (delete x2 after_ins))) = empty)

  let run_tests () =
    test_insert ();
    test_search ();
    test_getmax ();
    test_getmin ();
    test_delete ();
    ()

end

module IntTree = BinSTree(IntCompare)

let _ = IntTree.run_tests ()

(*****************************************************************************)
(*                               Part 3                                      *)
(*****************************************************************************)

(* A signature for a priority queue. *)
module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* What's being stored in the priority queue *)
  type elt

  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : queue -> bool

  (* Takes an element and a queue, and returns a new queue with the
   * element added *)
  val add : elt -> queue -> queue

  (* Pulls the highest priority element out of the passed-in queue,
   * also returning the queue with that element
   * removed. Can raise the QueueEmpty exception. *)
  val take : queue -> elt * queue

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(*>* Problem 3.0 *>*)

(* ListQueue: list-backed implementation of a priority queue *)
module ListQueue(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct

  exception QueueEmpty

  type elt = C.t

  type queue = elt list

(*>* Problem 3.1 *>*)
  let empty = []

(*>* Problem 3.2 *>*)
  let is_empty (t : queue) = 
    t = empty

(*>* Problem 3.3 *>*)

  (* add: adds the element to the list priority queue. If there are multiple
   * elements which compare equal, the most recently added element comes
   * later in the list *)
  let rec add (e : elt) (q : queue) = 
    match q with 
    | [] -> [e]
    | hd::tl ->
      match C.compare e hd with
      | Less -> e::q
      | _ -> hd::(add e tl)
    
(*>* Problem 3.4 *>*)

  (* take: removes the element with highest priority from the list priority 
   * queue *)
  let take (q : queue) = 
  match q with 
  | [] -> raise QueueEmpty
  | hd::tl -> (hd, tl)
  

  let test_add () =
    let x = C.generate () in
    let q = add x empty in
    assert (q = [x]);
    let y = C.generate_gt x () in
    let q = add y q in
    assert (q = [x;y]);
    let z = C.generate_lt x () in
    let q = add z q in
    assert (q = [z;x;y]);
    let w = C.generate () in
    let q = add w q in
    assert (q = [z;x;w;y]);
    ()

  let test_take () =
    let x = C.generate () in
    let q = add x empty in
    assert (take q = (x, []));
    let w = C.generate () in
    let q = add w q in
    assert (take q = (x, [w]));
    let y = C.generate_gt x () in
    let q = add y q in
    assert (take q = (x, [w;y]));
    let z = C.generate_lt x () in
    let q = add z q in
    assert (take q = (z, [x;w;y]));
    let q = add x q in
    assert (take q = (z, [x;w;x;y]));
    ()

  let test_is_empty () =
    let q = empty in
    assert (is_empty q);
    let q = add (C.generate ()) q in
    assert (not (is_empty q));
    ()

  (* run_tests: run invariant checks on this implementation of the list
   * priority queue *)
  let run_tests () = 
    test_add ();
    test_take ();
    test_is_empty ();
    ()
end

module IntLQueue = ListQueue(IntCompare)

let _ = IntLQueue.run_tests ()


(*>* Problem 3.5 *>*)

(* TreeQueue: tree-backed implementation of a priority queue *)
module TreeQueue(C : COMPARABLE) : PRIOQUEUE with type elt = C.t=
struct
  exception QueueEmpty

  module T = (BinSTree(C) : (BINTREE with type elt = C.t))
  
  (* What's being stored in the priority queue *)
  type elt = C.t

  (* The queue itself (stores things of type elt) *)
  type queue = T.tree

  (* Returns an empty queue *)
  let empty = T.empty

  (* is_empty: takes a queue, and returns whether or not it is empty *)
  let is_empty (t : queue) : bool =
     t = empty
    

  (* add: takes an element and a queue, and returns a new queue with the
   * element added *)
  let add (e : elt) (t : queue) : queue =
     T.insert e t

  (* take: pulls the highest priority element out of the passed-in queue,
   * also returning the queue with that element removed. Can raise the
   * QueueEmpty exception. *)
  let take (t : queue) : elt * queue =
    let m = T.getmin t in
    (m, T.delete m t)


  let test_add () =
    let x = C.generate () in
    let q = add x empty in
    assert (q = (T.insert x empty));
    let y = C.generate_gt x () in
    let q = add y q in
    assert (q = (T.insert y (T.insert x empty)));
    let z = C.generate_lt x () in
    let q = add z q in
    assert (q = (T.insert z (T.insert y (T.insert x empty))));
    let q = add x q in
    assert (q = (T.insert x (T.insert z (T.insert y (T.insert x empty)))));
    ()

  let test_take () =
    let x = C.generate () in
    let q = add x empty in
    assert (take q = (x,empty));
    let y = C.generate_gt x () in
    let q = add y q in
    assert (take q = (x,T.insert y empty));
    let z = C.generate_lt x () in
    let q = add z q in
    assert (take q = (z,T.insert y (T.insert x empty)));
    let q = add z q in
    assert (take q = (z,T.insert z (T.insert y (T.insert x empty))));
    ()

  let test_is_empty () =
    let q = empty in
    assert (is_empty q);
    let q = add (C.generate ()) q in
    assert (not (is_empty q));
    ()

  (* run_tests: run invariant checks on this implementation of the tree
   * priority queue *)
  let run_tests () = 
    test_add ();
    test_take ();
    test_is_empty ();
    ()
end

module IntTQueue = TreeQueue(IntCompare)

let _ = IntTQueue.run_tests ()


(*****************************************************************************)
(*                               Part 4                                      *)
(*****************************************************************************)

(*>* Problem 4.0 *>*)

(* BinaryHeap: binary min heap-backed implementation of a priority queue *)
module BinaryHeap(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct

  exception QueueEmpty

  type elt = C.t

  (* A node in the tree is either even or odd *)
  type balance = Even | Odd

  (* A tree is either just a single element, has one branch (the first elt in
   * the tuple is the element at this node, and the second elt is the element
   * down the branch), or has two branches (with the node being even or odd) *)
  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  (* A queue is either empty, or a tree *)
  type queue = Empty | Tree of tree

  let empty = Empty

  let is_empty (q : queue) =
    q = Empty

  (* add: adds element e to the queue q *)
  let add (e : elt) (q : queue) : queue =
    (* Given a tree, where e will be inserted is deterministic based on the
     * invariants. If we encounter a node in the tree where its value is greater
     * than the element being inserted, then we place the new elt in that spot
     * and propagate what used to be at that spot down toward where the new
     * element would have been inserted *)
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      (* If the tree is just a Leaf, then we end up with a OneBranch *)
      | Leaf e1 ->
        (match C.compare e e1 with
         | Equal | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))

      (* If the tree was a OneBranch, it will now be a TwoBranch *)
      | OneBranch(e1, e2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

      (* If the tree was even, then it will become an odd tree (and the element
       * is inserted to the left *)
      | TwoBranch(Even, e1, t1, t2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

      (* If the tree was odd, then it will become an even tree (and the element
       * is inserted to the right *)
      | TwoBranch(Odd, e1, t1, t2) ->
        match C.compare e e1 with
        | Equal | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
        | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
    in
    (* If the queue is empty, then e is the only Leaf in the tree.
     * Else, insert it into the proper location in the pre-existing tree *)
    match q with
    | Empty -> Tree (Leaf e)
    | Tree t -> Tree (add_to_tree e t)

  (* get_top: returns the top element of the tree t *)
  let get_top (t : tree) : elt = 
    match t with
      | Leaf e1 | OneBranch(e1, _) | TwoBranch(_, e1, _, _) -> e1

  
  (* replace_top: replaces the value of the top node in a tree *)
  let replace_top (e : elt) (t : tree) : tree =
    match t with
      | Leaf _ -> Leaf e
      | OneBranch(_, e2) -> OneBranch(e,e2)
      | TwoBranch(bal, _, t1, t2) -> TwoBranch(bal, e, t1, t2)

  (* fix: takes a tree, and if the top node is greater than its children, 
   * fixes it. If fixing it results in a subtree where the node is greater
   * than its children, then the subtree is recursively fixed *)
  let rec fix (t : tree) : tree = 
     match t with
      | Leaf _ -> t
      | OneBranch(e1, e2) ->
        (match C.compare e1 e2 with
         | Equal | Less -> t
         | Greater -> OneBranch(e2, e1))
      | TwoBranch(bal, e1, t1, t2) ->
        let top1 = get_top t1 in
        let top2 = get_top t2 in
        (match (C.compare e1 top1, C.compare e1 top2, C.compare top1 top2) with
         (* swap with left child *)
         | (Greater, _, Less) | (Greater, _, Equal) -> 
             TwoBranch(bal, top1, fix (replace_top e1 t1), t2)
         (* swap with right child *)
         | (_, Greater, Greater) -> 
             TwoBranch(bal, top2, t1, fix (replace_top e1 t2))
         | _ -> t)


  (* extract_tree: retrieves the tree from a queue *)
  let extract_tree (q : queue) : tree =
    match q with
    | Empty -> raise QueueEmpty
    | Tree t -> t

  (* get_last: takes a tree, and returns the item corresponding to the last 
   * node in the tree, as well as the queue that results from removing that
   * element. *)
  let rec get_last (t : tree) : elt * queue = 
     match t with 
     | Leaf e1 -> (e1, empty)
     | OneBranch(e1, e2) ->  (e2, Tree (Leaf e1))
     | TwoBranch(Even, e1, t1, t2) -> 
       let (last, rem) = get_last t2 in
       if is_empty rem then (last, Tree (OneBranch(e1, get_top t1)))
       else (last, Tree (TwoBranch(Odd, e1, t1, extract_tree rem))) 
     | TwoBranch(Odd, e1, t1, t2) -> 
       let (last, rem) = get_last t1 in
       if is_empty rem then (last, Tree (OneBranch(e1, get_top t2)))
       else (last, Tree (TwoBranch(Even, e1, extract_tree rem, t2)))

  (* take: takes a queue and returns the smallest element in the queue as well
     as the queue resulting from removing the element (the queue will satisfy
     the necessary invariants) *)
  let take (q : queue) : elt * queue =
    match extract_tree q with
    (* If the tree is just a Leaf, then return the value of that leaf, and the
     * new queue is now empty *)
    | Leaf e -> e, Empty

    (* If the tree is a OneBranch, then the new queue is just a Leaf *)
    | OneBranch (e1, e2) -> e1, Tree (Leaf e2)

    (* Removing an item from an even tree results in an odd tree. This
     * implementation replaces the root node with the most recently inserted
     * item, and then fixes the tree that results if it is violating the
     * strong invariant *)
    | TwoBranch (Even, e, t1, t2) ->
      let (last, q2') = get_last t2 in
      (match q2' with
       (* If one branch of the tree was just a leaf, we now have just
        * a OneBranch *)
       | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
       | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
    | TwoBranch (Odd, e, t1, t2) -> 
      let (last, q1') = get_last t1 in
      (match q1' with
       | Empty -> (e, Tree (fix (OneBranch (last, get_top t2))))
       | Tree t1' -> (e, Tree (fix (TwoBranch (Even, last, t1', t2)))))


  let test_add () =
    let x = C.generate () in
    let q = add x empty in
    assert (q = Tree (Leaf x));
    let q2 = add x q in 
    assert (q2 = Tree (OneBranch (x, x)));
    let y = C.generate_gt x () in
    let q3 = add y q in
    assert (q3 = Tree (OneBranch (x, y)));
    let z = C.generate_lt x () in
    let q4 = add z q in
    assert (q4 = Tree (OneBranch (z, x)));
    let q5 = add x q2 in 
    assert (q5 = Tree (TwoBranch (Even, x, Leaf x, Leaf x)));
    let q6 = add y q2 in 
    assert (q6 = Tree (TwoBranch (Even, x, Leaf x, Leaf y)));
    let q7 = add z q2 in 
    assert (q7 = Tree (TwoBranch (Even, z, Leaf x, Leaf x)));
    let q8 = Tree (TwoBranch (Even, z, Leaf x, Leaf y)) in
    let q9 = add z q8 in
    assert (q9 = Tree (TwoBranch (Odd, z, OneBranch (z, x), Leaf y)));
    let q10 = add y q8 in
    assert (q10 = Tree (TwoBranch (Odd, z, OneBranch (x, y), Leaf y))); 
    let zz = C.generate_lt z () in 
    let q11 = add zz q8 in
    assert (q11 = Tree (TwoBranch (Odd, zz, OneBranch (z,x), Leaf y)));
    let q12 = add z q10 in
    assert (q12 = Tree (TwoBranch (Even, z, OneBranch (x, y),
                                   OneBranch (z, y))));
    let q13 = add x q10 in
    assert (q13 = Tree (TwoBranch (Even, z, OneBranch (x, y),
                                   OneBranch (x, y))));
    let q14 = add zz q10 in
    assert (q14 = Tree (TwoBranch (Even, zz, OneBranch (x, y),
                                   OneBranch (z, y))));
    ()

  let test_is_empty () =
    let h = empty in
    assert (is_empty h);
    let h = add (C.generate ()) h in
    assert (not (is_empty h));
    ()
    
  let test_extract_tree () =  
    let x = C.generate () in
    assert (extract_tree (Tree (Leaf x)) = Leaf x);
    () 
    
  let test_get_top () =
    let x = C.generate () in 
    let q = add x empty in
    assert (get_top (extract_tree q) = x);
    let y = C.generate_gt x () in 
    let q = add y q in 
    assert (get_top (extract_tree q) = x);
    let z = C.generate_lt x () in 
    let q = add z q in
    assert (get_top (extract_tree q) = z);
    ()
    
  let test_fix () =
    let x = C.generate () in
    let q = add x empty in 
    assert (fix (extract_tree q) = (extract_tree q));
    assert (fix (OneBranch(x, x)) = OneBranch(x, x));
    let y = C.generate_gt x () in 
    let yy = C.generate_gt y () in
    assert (fix (OneBranch(x, y)) = OneBranch (x, y));
    assert (fix (OneBranch(y, x)) = OneBranch (x, y));
    let zx2 = C.generate_lt x () in
    let zx1 = C.generate_lt zx2 () in
    let z = C.generate_lt zx1 () in 
    assert (fix (TwoBranch(Even, x, Leaf z, Leaf y)) =
            TwoBranch(Even, z, Leaf x, Leaf y));
    assert (fix (TwoBranch(Even, x, Leaf z, Leaf z)) =
            TwoBranch(Even, z, Leaf x, Leaf z));
    assert (fix (TwoBranch(Even, x, Leaf y, Leaf z)) =
            TwoBranch(Even, z, Leaf y, Leaf x));
    assert (fix (TwoBranch(Odd, x, OneBranch(z, y), Leaf y)) =
            TwoBranch(Odd, z, OneBranch(x, y), Leaf y));
    assert (fix (TwoBranch(Odd, x, OneBranch(z, zx1), Leaf y)) =
            TwoBranch(Odd, z, OneBranch(zx1, x), Leaf y));
    assert (fix (TwoBranch(Odd, x, Leaf y, OneBranch(z, y))) =
            TwoBranch(Odd, z, Leaf y, OneBranch (x, y)));
    assert (fix (TwoBranch(Odd, x, Leaf y, OneBranch(z, zx1))) =
            TwoBranch(Odd, z, Leaf y, OneBranch(zx1, x)));
    assert (fix (TwoBranch(Even, x, TwoBranch (Even, z, Leaf zx1, Leaf zx2),
                           TwoBranch (Even, y, Leaf yy, Leaf yy))) = 
            TwoBranch(Even, z, fix (TwoBranch (Even, x, Leaf zx1, Leaf zx2)),
                      TwoBranch (Even, y, Leaf yy, Leaf yy)));
    assert (fix (TwoBranch(Even, x, TwoBranch (Even, y, Leaf yy, Leaf yy),
                           TwoBranch (Even, z, Leaf zx1, Leaf zx2))) = 
            TwoBranch(Even, z, TwoBranch (Even, y, Leaf yy, Leaf yy),
                      fix (TwoBranch (Even, x, Leaf zx1, Leaf zx2))));
    ()

  let test_get_last () =
    let w = C.generate () in
    let x = C.generate_gt w () in
    let y = C.generate_gt x () in
    let z = C.generate_gt y () in
    let q1 = add w empty in
    let q2 = add x q1 in
    let q3 = add y q2 in
    let q4 = add z q3 in
    assert (get_last (extract_tree q4) = (z, q3));
    assert (get_last (extract_tree q3) = (y, q2));
    assert (get_last (extract_tree q2) = (x, q1));
    assert (get_last (extract_tree q1) = (w, empty));
    ()

  let test_take () =
    let w = C.generate () in
    let x = C.generate_gt w () in
    let y = C.generate_gt x () in
    let z = C.generate_gt y () in
    let q = add w empty in
    let q = add x q in
    let q = add y q in
    let q = add z q in
    assert (q = Tree (TwoBranch(Odd, w, OneBranch(x,z), Leaf y)));
    assert (take q = (w, Tree (TwoBranch(Even, x, Leaf z, Leaf y))));
    assert (take (Tree (TwoBranch(Even, x, Leaf z, Leaf y))) =
            (x, Tree (OneBranch (y,z))));
    assert (take (Tree (OneBranch (y,z))) = (y, Tree (Leaf z)));
    assert (take (Tree (Leaf z)) = (z, empty));
    ()

  (* run_tests: run invariant checks on this implementation of the binary
   * heap priority queue *)
  let run_tests () = 
    test_add ();
    test_is_empty ();
    test_extract_tree ();
    test_get_top ();
    test_fix ();
    test_get_last ();
    test_take ();
end

module IntHeap = BinaryHeap(IntCompare)

let _ = IntHeap.run_tests ()


(* Priority Queues specifically to sort ints *)
module IntListQueue = (ListQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)
module IntHeapQueue = (BinaryHeap(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)
module IntTreeQueue = (TreeQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)


(* store the whole modules in these variables *)
let list_module = (module IntListQueue : PRIOQUEUE with type elt = IntCompare.t)
let heap_module = (module IntHeapQueue : PRIOQUEUE with type elt = IntCompare.t)
let tree_module = (module IntTreeQueue : PRIOQUEUE with type elt = IntCompare.t)


(* Implements sort using generic priority queues. *)
let sort (m : (module PRIOQUEUE with type elt=IntCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = IntCompare.t) in

  let rec extractor pq lst =
    if P.is_empty pq then lst else
    let (x, pq') = P.take pq in
    extractor pq' (x::lst) in
  let pq = List.fold_right ~f:P.add ~init:P.empty lst in
  List.rev (extractor pq [])


(* various sorting algorithms based on underlying data structure *)
let heapsort = sort heap_module
let treesort = sort tree_module
let selectionsort = sort list_module

assert (heapsort [] = [])
assert (heapsort [3] = [3])
assert (heapsort [2;4;5;3;1;1;2] = [1;1;2;2;3;4;5])
assert (selectionsort [] = [])
assert (selectionsort [3] = [3])
assert (selectionsort [2;4;5;3;1;1;2] = [1;1;2;2;3;4;5])
assert (treesort [] = [])
assert (treesort [3] = [3])
assert (treesort [2;4;5;3;1;1;2] = [1;1;2;2;3;4;5])


(*****************************************************************************)
(*                               Part N                                      *)
(*****************************************************************************)

(*>* Problem N.0 *>*)

(* CompSorter: takes a COMPARABLE module and allow sorting on the type defined
   by that module *)
module CompSorter (C : COMPARABLE) =
struct

  type c = C.t
  
  (* sort: uses a binary heap-backed priority queue to sort *)
  let sort (lst : c list) : c list = 
    let module P = (BinaryHeap(C) : PRIOQUEUE with type elt = c) in
    let rec extractor pq lst =
      if P.is_empty pq then lst else
      let (x, pq') = P.take pq in
      extractor pq' (x::lst) in
  let pq = List.fold_right ~f:P.add ~init:P.empty lst in
  List.rev (extractor pq [])

end
   

(*>* Problem N.1 *>*)

Random.self_init()

(* gen_list: generates a random list of integers of a certain length *)
let rec gen_list (len : int) (lst : int list) : int list = 
  if len = 0 then lst
  else gen_list (len - 1) ((Random.int Int.max_value) :: lst)
  
(* time_to_sort: returns the amount of time taken to apply the given sorting
   algorithm to the given list *)
let time_to_sort (sortalg : int list -> int list) (lst : int list) : float =
  let start = Unix.gettimeofday() in
  let _ = sortalg lst in
  Unix.gettimeofday() -. start

(* test_sorts: tests and prints the times for running the sorting algorithms 
   on lists of varying lengths *)
let test_sorts (min : int) (max : int) (seq : int) : unit = 
  let rec test_all sortalg cur = 
     if cur > max then ()
     else
       let _ = print_int cur in
       let _ = print_char ' ' in
       let _ = print_float (time_to_sort sortalg (gen_list cur [])) in
       let _ = print_newline () in
       test_all sortalg (cur + seq)
  in
  let _ = print_endline "Heapsort:" in
  let _ = test_all heapsort min in
  let _ = print_endline "Tree sort:" in
  let _ = test_all treesort min in
  let _ = print_endline "Selection sort:" in
  let _ = test_all selectionsort min in
  ()


(* actually test the sorts *)
(* let _ = test_sorts 1000 50000 1000 *)

(* Based on plots of the raw results and fitting x^2 and x*log(x) curves
 * to the data,it appears that the complexity of the average run times for
 * the sorting algorithms are:
 * Heapsort: O(n log n)
 * Tree sort: O(n log n)
 * Selection sort: O(n^2) *)

(* Raw results: 
Heapsort:
1000 0.00138187408447
2000 0.00209307670593
3000 0.00386905670166
4000 0.00530004501343
5000 0.0123028755188
6000 0.00992012023926
7000 0.011255979538
8000 0.0192210674286
9000 0.0224390029907
10000 0.0201499462128
11000 0.0261361598969
12000 0.0271270275116
13000 0.0338878631592
14000 0.0321710109711
15000 0.0377631187439
16000 0.0370440483093
17000 0.0424499511719
18000 0.0455429553986
19000 0.064836025238
20000 0.054486989975
21000 0.0578501224518
22000 0.0651259422302
23000 0.0669701099396
24000 0.0673670768738
25000 0.0644872188568
26000 0.0708930492401
27000 0.0748109817505
28000 0.0807960033417
29000 0.0966742038727
30000 0.101626873016
31000 0.0929250717163
32000 0.0993180274963
33000 0.0950729846954
34000 0.0931529998779
35000 0.0968508720398
36000 0.108718156815
37000 0.111907958984
38000 0.105221033096
39000 0.116711854935
40000 0.120548963547
41000 0.132971048355
42000 0.136343002319
43000 0.132279872894
44000 0.154407978058
45000 0.140086889267
46000 0.144338846207
47000 0.150900840759
48000 0.162857055664
49000 0.17184305191
50000 0.162599086761
Tree sort:
1000 0.000500917434692
2000 0.00146698951721
3000 0.00209403038025
4000 0.00383806228638
5000 0.00980186462402
6000 0.00673294067383
7000 0.0103678703308
8000 0.0105588436127
9000 0.00922894477844
10000 0.0198078155518
11000 0.0191349983215
12000 0.0145788192749
13000 0.0199820995331
14000 0.0262401103973
15000 0.0271701812744
16000 0.0283939838409
17000 0.0305998325348
18000 0.0312101840973
19000 0.0293769836426
20000 0.0361201763153
21000 0.0401339530945
22000 0.0385739803314
23000 0.0404191017151
24000 0.0452520847321
25000 0.0417129993439
26000 0.0498650074005
27000 0.0477612018585
28000 0.0529088973999
29000 0.0506072044373
30000 0.055704832077
31000 0.054447889328
32000 0.0654489994049
33000 0.0605380535126
34000 0.0628960132599
35000 0.064532995224
36000 0.0650789737701
37000 0.0652401447296
38000 0.0675201416016
39000 0.0697658061981
40000 0.0697340965271
41000 0.0811660289764
42000 0.0793128013611
43000 0.0785539150238
44000 0.0823481082916
45000 0.0789310932159
46000 0.0813369750977
47000 0.0842690467834
48000 0.0885331630707
49000 0.0940778255463
50000 0.0913178920746
Selection sort:
1000 0.00597596168518
2000 0.0285320281982
3000 0.0610699653625
4000 0.110477924347
5000 0.167448043823
6000 0.237142086029
7000 0.340881109238
8000 0.474040031433
9000 0.574506998062
10000 0.721822023392
11000 0.859656095505
12000 1.02237892151
13000 1.35468912125
14000 1.74720907211
15000 1.74593901634
16000 2.05303883553
17000 2.30783009529
18000 2.70913720131
19000 3.00321602821
20000 3.8345990181
21000 4.62922406197
22000 4.81661081314
23000 6.15074586868
24000 6.01404094696
25000 5.76302695274
26000 6.6162648201
27000 6.35800695419
28000 7.20544409752
29000 7.51932406425
30000 8.55282092094
31000 8.86711192131
32000 9.79116201401
33000 10.7192161083
34000 10.8278110027
35000 11.5359799862
36000 12.5026462078
37000 13.0773460865
38000 14.4706819057
39000 14.8201458454
40000 16.3561911583
41000 17.6223940849
42000 19.1826729774
43000 18.9950928688
44000 20.3223519325
45000 21.5616219044
46000 23.0684869289
47000 22.9640469551
48000 24.0982408524
49000 26.7836000919
50000 27.7786591053
 *)


(*>* Problem N.2 *>*)
let minutes_spent : int = 1000
