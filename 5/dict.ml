open Core.Std

(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  In our case, we will
 * be using a dictionary to build an index for the web, associating
 * a set of URLs with each word that we find as we crawl the web.
 *)
exception TODO

module type DICT =
sig
  type key
  type value
  type dict

  (* An empty dictionary *)
  val empty : dict

  (* Reduce the dictionary using the provided function f and base case u.
   * Our reducing function f must have the type:
   *      key -> value -> 'a -> 'a
   * and our base case u has type 'a.
   *
   * If our dictionary is the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold should return:
   *      f k1 v1 (f k2 v2 (f k3 v3 (f ... (f kn vn u))))
   *)
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is not in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  (* Runs all the tests. see TESTING EXPLANATION below *)
  val run_tests : unit -> unit
end



(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Ordering.t
  val string_of_key : key -> string
  val string_of_value : value -> string

  (* Use these functions for testing. See TESTING EXPLANATION. *)

  (* Generate a key. The same key is always returned *)
  val gen_key : unit -> key

  (* Generate a random key. *)
  val gen_key_random : unit -> key

  (* Generates a key greater than the argument. *)
  val gen_key_gt : key -> unit -> key

  (* Generates a key less than the argument. *)
  val gen_key_lt : key -> unit -> key

  (* Generates a key between the two arguments. Return None if no such
   * key exists. *)
  val gen_key_between : key -> key -> unit -> key option

  (* Generates a random value. *)
  val gen_value : unit -> value

  (* Generates a random (key,value) pair *)
  val gen_pair : unit -> key * value
end



(* An example implementation of our DICT_ARG signature. Use this struct
 * for testing. *)
module IntStringDictArg : DICT_ARG =
struct
  open Order
  type key = int
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_key = string_of_int
  let string_of_value v = v
  let gen_key () = 0
  let gen_key_gt x () = x + 1
  let gen_key_lt x () = x - 1
  let gen_key_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)

  (* returns the nth string in lst, or "cow" n > length of list *)
  let rec lst_n (lst: string list) (n: int) : string =
    match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)

  (* list of possible values to generate *)
  let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                         "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                         "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]
  let num_values = List.length possible_values
  (* gen_value will return the string at this current index *)
  let current_index = ref 0
  let gen_value () =
    let index = !current_index in
    if index >= num_values then
      (current_index := 0; lst_n possible_values index)
    else
      (current_index := index + 1; lst_n possible_values index)
  let gen_pair () = (gen_key_random(), gen_value())
end



(* An association list implementation of our DICT signature. *)
module AssocListDict(D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
struct
  open Order;;
  type key = D.key;;
  type value = D.value;;
  type dict = (key * value) list;;

  (* INVARIANT: sorted by key, no duplicates *)

  let empty = [] ;;

  let fold f d = List.fold_left ~f:(fun a (k, v) -> f k v a) ~init:d

  let rec lookup d k =
    match d with
      | [] -> None
      | (k1,v1)::d1 ->
        (match D.compare k k1 with
          | Equal -> Some v1
          | Greater -> lookup d1 k
          | _ -> None)

  let member d k =
    match lookup d k with
      | None -> false
      | Some _ -> true

  let rec insert d k v =
    match d with
      | [] -> [(k,v)]
      | (k1,v1)::d1 ->
        (match D.compare k k1 with
          | Less -> (k,v)::d
          | Equal -> (k,v)::d1
          | Greater -> (k1,v1)::(insert d1 k v))

  let rec remove d k =
    match d with
      | [] -> []
      | (k1,v1)::d1 ->
	(match D.compare k k1 with
          | Equal -> d1
          | Greater -> (k1,v1)::(remove d1 k)
          | _ -> d)

  let choose d =
    match d with
      | [] -> None
      | (k,v)::rest -> Some(k,v,rest)

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string =
    let f = (fun y (k,v) -> y ^ "\n key: " ^ D.string_of_key k ^
      "; value: (" ^ D.string_of_value v ^ ")") in
    List.fold_left ~f:f ~init:"" d

  (****************************************************************)
  (* Tests for our AssocListDict functor                          *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left lst ~f:(fun r (k,v) -> insert r k v) ~init:d

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right lst ~f:(fun (k,v) r -> insert r k v) ~init:d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_insert () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter pairs1 ~f:(fun (k,v) -> assert(lookup d1 k = Some v)) ;
    ()

  let test_remove () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        List.iter
          pairs1
          ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          )
      );
    ()

  let test_lookup () =
    ()

  let test_choose () =
    ()

  let test_member () =
    ()

  let test_fold () =
    ()

  let run_tests () =
    test_insert() ;
    test_remove() ;
    test_lookup() ;
    test_choose() ;
    test_member() ;
    test_fold() ;
    ()

end



(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)                              *)
(******************************************************************)

module BTDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  open Order

  exception TODO

  type key = D.key
  type value = D.value

  (* A dictionary entry is a (key,value) pair. We compare two (key,value)
   * pairs with the provided key-comparison function D.compare. For example,
   * we may choose to keep a dictionary mapping links to their ranks. In this
   * case, our (key,value) pairs will be (link,rank) pairs, and we compare
   * links using string comparison. *)
  type pair = key * value

  (* Type definition for dictionary, which we choose to represent as a 2-3 Tree.
   * This is almost the same as the binary search tree definition from pset4 and
   * lecture, except we add one more case: a Three-node.
   *
   * A Three-node contains two pairs and three subtrees: left, middle, and
   * right, represented by the 3 dicts in the definition below. *)
  type dict =
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  (* INVARIANTS:
   * 2-node: Two(left,(k1,v1),right)
   * (1) Every key k appearing in subtree left must be k < k1.
   * (2) Every key k appearing in subtree right must be k > k1.
   * (3) The length of the path from the 2-node to
   *     every leaf in its two subtrees must be the same.
   *
   * 3-node: Three(left,(k1,v1),middle,(k2,v2),right)
   * (1) k1 < k2.
   * (2) Every key k appearing in subtree left must be k < k1.
   * (3) Every key k appearing in subtree right must be k > k2.
   * (4) Every key k appearing in subtree middle must be k1 < k < k2.
   * (5) The length of the path from the 3-node to every leaf in its three
   *     subtrees must be the same.
   *)

  (* FOR INSERTION:
   * A kicked configuration returned by going downwards on insertion.
   * We can only kick up Two nodes, hence Up takes a dict * pair * dict *)
  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  (* FOR REMOVAL:
   * A hole configuration returned by going downwards on removal. We
   * include a pair option whenever we remove the minimum of the right
   * subtree of the current pair in order the current pair *)
  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  (* FOR REMOVAL:
   * A direction will distinguish which configuration we came from in the
   * removal cases. We use direction2 for cases (1-2) on the handout, and
   * we use direction3 for cases (3-4) on the handout. *)
  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3

  (* How do we represent an empty dictionary with 2-3 trees? *)
  let empty : dict = Leaf

  (* Upward phase for w where its parent is a Two node whose (key,value) is x.
   * One of x's children is w, and the other child is x_other. This function
   * should return a kicked-up configuration containing the new tree as a
   * result of performing the upward phase on w. *)
  let insert_upward_two (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (x_other: dict) : kicked =
    let (wk, _) = w in
    let (xk, _) = x in
    match D.compare wk xk with
    | Less -> Done (Three (w_left, w, w_right, x, x_other))
    | Equal -> failwith "Duplicate key"
    | Greater -> Done (Three (x_other, x, w_left, w, w_right))

  (* Upward phase for w where its parent is a Three node whose (key,value) is x.
   * One of x's children is w, and of the two remaining children,
   * other_left is the subtree more to the left and other_right is the
   * subtree more to the right.
   * This function should return a kicked-up configuration containing the
   * new tree as a result of performing the upward phase on w. *)
  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
    let (wk, _) = w in
    let (xk, _) = x in
    let (yk, _) = y in
    match (D.compare wk xk, D.compare wk yk) with
    | (Less, _) -> Up (Two (w_left, w, w_right), x,
                       Two (other_left, y, other_right))
    | (_, Greater) -> Up (Two (other_left, x, other_right), y,
                          Two (w_left, w, w_right))
    | _ -> Up (Two (other_left, x, w_left), w,
               Two (w_right, y, other_right))

  (* Downward phase for inserting (k,v) into our dictionary d.
   * The downward phase returns a "kicked" up configuration, where
   *
   * type kicked =
   *      | Up of dict * pair * dict
   *      | Done of dict
   *
   * A kicked up configuration can only be a Two node, hence the Up
   * constructor takes the same parameters as the Two constructor. We return
   * Up(left,(k,v),right) if the Two-node represented by this Up needs to
   * be further kicked up in the upward phase (this is represented by an up
   * arrow on the 2-3 Tree handout). We return Done(d) if we have finished
   * our upward phase on the tree represented by d. *)

  (* insert_downward should handle the base case when inserting into a Leaf,
   * and if our dictionary d is a Two-node or a Three-node, we call the
   * corresponding functions insert_downward_two or insert_downward_three
   * with the appropriate arguments. *)
  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up (Leaf, (k, v), Leaf)
      | Two(left,n,right) -> insert_downward_two (k,v) n left right
      | Three(left,n1,middle,n2,right) -> insert_downward_three (k,v) n1 n2
                                            left middle right

  (* Downward phase on a Two node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) is the (key,value) of the current Two node, and left and right
   * are the two subtrees of the current Two node. *)
  and insert_downward_two ((k,v): pair) ((k1,v1): pair)
      (left: dict) (right: dict) : kicked =
    match D.compare k k1 with
    | Less -> 
      (match insert_downward left k v with
       | Done t -> Done (Two (t, (k1, v1), right))
       | Up (l, w, r) -> insert_upward_two w l r (k1, v1) right)
    | Equal -> Done (Two (left, (k, v), right))
    | Greater -> 
      (match insert_downward right k v with
       | Done t -> Done (Two (left, (k1, v1), t))
       | Up (l, w, r) -> insert_upward_two w l r (k1, v1) left)

  (* Downward phase on a Three node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) and (k2,v2) are the two (key,value) pairs in our Three node, and
   * left, middle, and right are the three subtrees of our current Three node *)
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : kicked =
    match (D.compare k k1, D.compare k k2) with
    | (Less, _) -> 
      (match insert_downward left k v with
       | Done t -> Done (Three (t, (k1, v1), middle, (k2, v2), right))
       | Up (l, w, r) -> insert_upward_three w l r 
                           (k1, v1) (k2, v2) middle right)
    | (Equal, _) -> Done (Three (left, (k, v), middle, (k2, v2), right))
    | (_, Greater) ->
      (match insert_downward right k v with
       | Done t -> Done (Three (left, (k1, v1), middle, (k2, v2), t))
       | Up (l, w, r) -> insert_upward_three w l r
                           (k1, v1) (k2, v2) left middle)
    | (_, Equal) -> Done (Three (left, (k1, v1), middle, (k, v), right))
    | (Greater, Less) ->
      (match insert_downward middle k v with
       | Done t -> Done (Three (left, (k1, v1), t, (k2, v2), right))
       | Up (l, w, r) -> insert_upward_three w l r
                           (k1, v1) (k2, v2) left right)

  (* We insert (k,v) into our dict using insert_downward, which gives us
   * "kicked" up configuration. We return the tree contained in the "kicked"
   * configuration. *)
  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x

  (* Upward phase for removal where the parent of the hole is a Two node.
   * See cases (1-2) on the handout. n is the (key,value) pair contained in
   * the parent node; left and right are the subtrees of the parent node (our
   * hole is one of these subtrees); and dir indicates which subtree was
   * contained by the hole. *)
  let remove_upward_two (n: pair) (rem: pair option)
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) ->
   Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d ->
   Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  (* Upward phase for removal where the parent of the hole is a Three node.
   * See cases (3-4) on the handout. n1 and n2 are the (key,value) pairs
   * contained in the parent node; left, middle, and right are the subtrees
   * of the parent node (our hole is one of these subtrees); and dir indicates
   * which subtree was the tree contained by the hole. *)
  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e ->
   Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e ->
   Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) ->
   Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e ->
   Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  (* DO NOT EDIT THIS *)
  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Equal -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Equal, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Equal -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  (* DO NOT EDIT THIS *)
  and remove_downward_two (k: key) ((k1,v1): pair)
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) ->
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less ->
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  (* DO NOT EDIT THIS *)
  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Equal, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) ->
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) ->
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) ->
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) ->
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  (* DO NOT EDIT THIS *)
  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  (* DO NOT EDIT THIS *)
  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  (* Applies a function all key value pairs in a dict and accumulates the
   * results. *)
  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
    | Leaf -> u
    | Two (_, (k, v), _) | Three (_, (k, v), _, _, _) -> 
      f k v (fold f u (remove d k))

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string = 
    fold (fun k v s ->
            "(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ ") "
            ^ s) "" d

  (* Debugging function. This will print out the tree in text format.
   * Use this function to see the actual structure of your 2-3 tree. *)
  let rec string_of_tree (d: dict) : string =
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ ","
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"

  (* Returns the value of the given key in our dictionary and returns it as
   * an option, or return None if the key is not in our dictionary. *)
  let rec lookup (d: dict) (k: key) : value option =
    match d with
    | Leaf -> None
    | Two (l, (k1, v1), r) -> 
      (match D.compare k k1 with
       | Equal -> Some v1
       | Less -> lookup l k
       | Greater -> lookup r k)
    | Three (l, (k1, v1), m, (k2, v2), r) ->
      (match (D.compare k k1, D.compare k k2) with
       | (Equal, _) -> Some v1
       | (_, Equal) -> Some v2
       | (Less, _) -> lookup l k
       | (_, Greater) -> lookup r k
       | (Greater, Less) -> lookup m k)

  (* Tests if a given key is in our dictionary *)
  let member (d: dict) (k: key) : bool =
    match lookup d k with
    | None -> false
    | _ -> true

  (* Removes any (key,value) pair from our dictionary, and returns
   * as an option this (key,value) pair along with the new dictionary.
   * If our dictionary is empty, this should return None. *)
  let choose (d: dict) : (key * value * dict) option =
    match d with
    | Leaf -> None
    | Two (_, (k,v), _) | Three (_, (k,v), _, _, _) -> Some (k, v, remove d k)

  (* Given a 2-3 tree (represented by our dictionary d), returns true if
   * and only if the tree is "balanced" *)

  (* How are you testing that you tree is balanced?
   * ANSWER:
   *    We use a helper function that returns whether a tree is balanced as 
   *    well as the depth of the tree, and a tree is balanced if all
   *    subtrees are balanced and of the same depth
   *)
  let balanced (d: dict) : bool =
    let rec balanced_depth di : bool * int = 
      match di with 
      | Leaf -> (true, 1)
      | Two (l, _, r) -> 
        let (l_bal, l_dep) = balanced_depth l in
        let (r_bal, r_dep) = balanced_depth r in
        (l_bal && r_bal && l_dep = r_dep, l_dep + 1)
      | Three (l, _, m, _, r) ->
        let (l_bal, l_dep) = balanced_depth l in
        let (m_bal, m_dep) = balanced_depth m in
        let (r_bal, r_dep) = balanced_depth r in
        (l_bal && m_bal && r_bal && l_dep = m_dep && m_dep = r_dep, l_dep + 1)
    in
    let (bal,_) = balanced_depth d in
    bal

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left lst ~f:(fun r (k,v) -> insert r k v) ~init:d

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right lst ~f:(fun (k,v) r -> insert r k v) ~init:d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))


  let test_balance () =
    let d1 = Leaf in
    assert(balanced d1);

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    assert(balanced d2);

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    assert(balanced d3);

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    assert(balanced d4);

    let d5 = Two(Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf)) in
    assert(not (balanced d5));

    let d6 = Three(Leaf,D.gen_pair(),
                   Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),Leaf) in
    assert(not (balanced d6));

    let d7 = Three(Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf),
                   D.gen_pair(),Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf))
    in
    assert(not (balanced d7));
    ()

  let test_fold () = 
    let elts1 = generate_random_list 100 in
    let d1 = insert_list empty elts1 in
    assert(balanced d1);
    assert(fold (fun k v a -> (List.exists ~f:(fun x -> x=(k,v)) elts1) && a)
             true d1);
    assert(not (fold (fun k v a ->
                        (List.exists ~f:(fun x -> x=(k,v)) elts1) && a)
                  false d1));
    ()

  let test_lookup () = 
    let elts1 = generate_pair_list 100 in
    let d1 = insert_list empty elts1 in
    assert(balanced d1);
    assert(List.fold_left ~f:(fun t (k,v) -> 
                                t &&
                                match lookup d1 k with
                                | None -> false
                                | Some v1 -> v1 = v) ~init:true elts1);
    ()


  let test_member () =
    let elts = generate_random_list 100 in
    List.iter elts ~f:(fun (k, _) -> assert(not (member empty k)));
    let d1 = insert_list empty elts in
    assert(balanced d1);
    List.iter elts ~f:(fun (k, _) -> assert(member d1 k));
  ()
    
  let test_insert () =
    let d1 = Leaf in
    assert(balanced d1);

    let value = D.gen_value() in
    let key1 = D.gen_key() in
    let pair1 = (key1, D.gen_value ()) in
    let key2 = D.gen_key_gt key1 () in
    let pair2 = (key2, D.gen_value ()) in
    let key3 = D.gen_key_gt key2 () in
    let pair3 = (key3, D.gen_value ()) in
    let key4 = D.gen_key_gt key3 () in
    let pair4 = (key4, D.gen_value ()) in
    let key5 = D.gen_key_gt key4 () in
    let pair5 = (key5, D.gen_value ()) in
    let key6 = D.gen_key_gt key5 () in
    let pair6 = (key6, D.gen_value ()) in
    let key7 = D.gen_key_gt key6 () in
    let pair7 = (key7, D.gen_value ()) in
    let key8 = D.gen_key_gt key7 () in
    let pair8 = (key8, D.gen_value ()) in
    let key9 = D.gen_key_gt key8 () in
    
    let twotree = Two(Two(Leaf,pair2,Leaf),pair3,Two(Leaf,pair4,Leaf)) in
    assert(balanced twotree);
    let twotreereplace = insert twotree key2 value in
    assert(balanced twotreereplace);
    assert(twotreereplace =
           Two(Two(Leaf,(key2, value),Leaf),pair3,Two(Leaf,pair4,Leaf)));
    let twotreeless = insert twotree key1 value in
    assert(balanced twotreeless);
    assert(twotreeless =
           Two(Three(Leaf, (key1,value) , Leaf, pair2, Leaf), pair3,
               Two(Leaf, pair4,Leaf)));
    let twotreegreater = insert twotree key5 value in
    assert(balanced twotreegreater);
    assert(twotreegreater =
           Two(Two(Leaf,pair2,Leaf), pair3,
               Three(Leaf, pair4, Leaf, (key5, value), Leaf)));
    
    let threetree = Three(Two(Leaf,pair2,Leaf),pair3,Two(Leaf,pair5,Leaf),
                          pair7,Two(Leaf,pair8,Leaf)) in
    assert(balanced threetree);
    let threetreeless = insert threetree key1 value in
    assert(balanced threetreeless);
    assert(threetreeless = 
           Three(Three(Leaf, (key1,value), Leaf, pair2, Leaf), pair3,
                 Two(Leaf,pair5,Leaf),pair7, Two(Leaf,pair8,Leaf)));
    let threetreemiddle = insert threetree key4 value in
    assert(balanced threetreemiddle);
    assert(threetreemiddle =
           Three(Two(Leaf,pair2,Leaf),pair3,
                 Three(Leaf,(key4,value),Leaf,pair5,Leaf),pair7,
                 Two(Leaf,pair8,Leaf)));
    let threetreegreater = insert threetree key9 value in
    assert(balanced threetreegreater);
    assert(threetreegreater =
           Three(Two(Leaf,pair2,Leaf),pair3,Two(Leaf,pair5,Leaf),pair7,
                 Three(Leaf, pair8, Leaf, (key9, value), Leaf)));
    
    let threetree2 = Three(Three(Leaf, pair2, Leaf, pair3, Leaf),pair4,
                           Two(Leaf,pair5,Leaf),pair6,Two(Leaf,pair7,Leaf)) in
    assert(balanced threetree2);
    let threetree2left = insert threetree2 key1 value in
    assert(balanced threetree2left);
    assert(threetree2left =
           Two(Two(Two(Leaf,(key1, value),Leaf),pair2,Two(Leaf,pair3,Leaf)),
               pair4,Two(Two(Leaf,pair5,Leaf),pair6,Two(Leaf,pair7,Leaf))));
    let threetree3 = Three(Two(Leaf,pair1,Leaf),pair2,
                           Three(Leaf, pair4, Leaf, pair5, Leaf),pair6,
                           Two(Leaf,pair7,Leaf)) in
    assert(balanced threetree3);
    let threetree3middle = insert threetree3 key3 value in
    assert(balanced threetree3middle);
    assert(threetree3middle =
           Two(Two(Two(Leaf,pair1,Leaf),pair2,Two(Leaf,(key3, value),Leaf)),
               pair4,Two(Two(Leaf,pair5,Leaf),pair6,Two(Leaf,pair7,Leaf))));
    let threetree4 = Three(Two(Leaf,pair1,Leaf),pair2,
                           Three(Leaf, pair3, Leaf, pair5, Leaf),pair6,
                           Two(Leaf,pair7,Leaf)) in
    assert(balanced threetree4);
    let threetree4middle = insert threetree4 key4 value in
    assert(balanced threetree4middle);
    assert(threetree4middle = 
           Two(Two(Two(Leaf,pair1,Leaf),pair2,Two(Leaf,pair3,Leaf)),
               (key4, value),Two(Two(Leaf,pair5,Leaf),pair6,
                                 Two(Leaf,pair7,Leaf))));
    let threetree5 = Three(Two(Leaf,pair1,Leaf),pair2,Two(Leaf,pair3,Leaf),
                           pair4,Three(Leaf, pair5, Leaf, pair6, Leaf)) in
    assert(balanced threetree5);
    let threetree5middle = insert threetree5 key7 value in
    assert(balanced threetree5middle);
    assert(threetree5middle =
           Two(Two(Two(Leaf,pair1,Leaf),pair2,Two(Leaf,pair3,Leaf)),pair4,
               Two(Two(Leaf,pair5,Leaf),pair6,Two(Leaf,(key7, value),Leaf))));
  ()
    

  let test_remove_nothing () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    assert(balanced d1);
    let r2 = remove d1 (D.gen_key_lt (D.gen_key()) ()) in
    List.iter pairs1 ~f:(fun (k,v) -> assert(lookup r2 k = Some v)) ;
    assert(balanced r2) ;
    ()

  let test_remove_from_nothing () =
    let d1 = empty in
    let r1 = remove d1 (D.gen_key()) in
    assert(r1 = empty) ;
    assert(balanced r1) ;
    ()

  let test_remove_in_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    assert (balanced d1);
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        let _ = List.iter
          pairs1
          ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          )
        in
        assert(balanced r)
      ) ;
    ()

  let test_remove_reverse_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    assert(balanced d1);
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        let _ = List.iter
          pairs1 ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) in
        assert(balanced r)
      ) ;
    ()

  let test_remove_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    assert(balanced d5);
    let r5 = List.fold_right pairs5 ~f:(fun (k,_) d -> remove d k) ~init:d5 in
    List.iter pairs5 ~f:(fun (k,_) -> assert(not (member r5 k))) ;
    assert(r5 = empty) ;
    assert(balanced r5) ;
    ()

  let rec choose_until_empty (size : int) (d: dict): (int*bool) = 
    match choose d with
    | None -> (size, true)
    | Some(k, _, d1) -> let (size1, bool1) = (choose_until_empty (size+1) d1) 
                        in 
                        (size1, (member d k) && bool1 && not (member d1 k))
    
  let test_choose () = 
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    assert(balanced d1);
    List.iter pairs1 ~f:(fun _ -> assert(not ((choose d1) = None )));
    assert((choose_until_empty 0 d1) = (26, true));
    assert((choose empty) = None);
    ()


  let run_tests () =
    test_balance() ;
    test_fold() ;
    test_lookup() ;
    test_member() ;
    test_insert() ; 
    test_remove_nothing() ;
    test_remove_from_nothing() ;
    test_remove_in_order() ;
    test_remove_reverse_order() ;
    test_remove_random_order() ; 
    test_choose() ;
    ()

end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a dictionary mapping ints to strings using our
 * AssocListDict functor and run the tests *)
module IntStringListDict = AssocListDict(IntStringDictArg) ;;
IntStringListDict.run_tests();;

(* Create a dictionary mapping ints to strings using our
 * BTDict functor and run the tests.
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 tree implementation. *)
module IntStringBTDict = BTDict(IntStringDictArg) ;;
IntStringBTDict.run_tests();;



(******************************************************************)
(* Make: a functor that creates a DICT by calling our             *)
(* AssocListDict or BTDict functors                               *)
(******************************************************************)
module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
  (* Change this line to the BTDict implementation when you are
   * done implementing your 2-3 trees. *)
  (* AssocListDict(D) *)
  BTDict(D)

let minutes_spent = 1000
