open Core.Std
open Util
open CrawlerServices
open Order
open Nodescore
open Graph


(* Dictionaries mapping links to their ranks. Higher is better. *)
module RankDict = Dict.Make(
  struct
    type key = link
    type value = float
    let compare = link_compare
    let string_of_key = string_of_link
    let string_of_value = Float.to_string
    let gen_key () = {host=""; port=0; path=""}
    let gen_key_gt _ () = gen_key ()
    let gen_key_lt _ () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between _ _ () = None
    let gen_value () = 0.0
    let gen_pair () = (gen_key(),gen_value())
  end)

module PageSet = Myset.Make(
  struct
    type t = page
    let compare = (fun a b -> link_compare (a.url) (b.url))
    let string_of_t = string_of_page
    let gen () = {url={host=""; port=0; path=""}; links=[]; words=[]}
    let gen_lt _ () = gen ()
    let gen_gt _ () = gen ()
    let gen_random () = gen ()
    let gen_between _ _ () = None
  end)

module LinkSet = Myset.Make(
  struct
    type t = link
    let compare = link_compare
    let string_of_t = string_of_link
    let gen () = {host=""; port=0; path=""}
    let gen_lt _ () = gen ()
    let gen_gt _ () = gen ()
    let gen_random () = gen ()
    let gen_between _ _ () = None
  end)

module PageGraph = Graph (
  struct
    type node = link
    let compare = link_compare
    let string_of_node = string_of_link
    let gen () = {host=""; port=0; path=""}
  end)

module PageScore = NodeScore (
  struct
    type node = link
    let string_of_node = string_of_link
    let compare = link_compare
    let gen () = {host=""; port=0; path=""}
  end)

(* Given a bunch of pages, convert them to a graph *)
let graph_of_pages (pages : PageSet.set) : PageGraph.graph =
  (* Only want graph nodes for pages we actually crawled *)
  let crawled_links =
    PageSet.fold (fun page s -> LinkSet.insert page.url s)
      LinkSet.empty pages
  in
  let add_links page graph =
    let add_link g dst =
      if LinkSet.member crawled_links dst then
        PageGraph.add_edge g page.url dst
      else g
    in
      List.fold_left page.links ~f:add_link ~init:graph
  in
    PageSet.fold add_links PageGraph.empty pages

(* The rest of the world wants a RankDict, not a NodeScore. *)

let dict_of_ns (ns : PageScore.node_score_map) : RankDict.dict =
  PageScore.fold (fun node score r -> RankDict.insert r node score)
    RankDict.empty ns

(* A type for modules that can compute nodescores from graphs *)
module type RANKER =
sig
  module G: GRAPH
  module NS: NODE_SCORE
  val rank : G.graph -> NS.node_score_map
end


(* Each node's rank is equal to the number of pages that link to it. *)
module InDegreeRanker  (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N) :
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA
  let rank (g : G.graph) =
    let add_node_edges ns node =
      let neighbors = match G.neighbors g node with
        | None -> []
        | Some xs -> xs
      in
        List.fold_left neighbors ~f:(fun ns' neighbor -> NS.add_score ns' neighbor 1.0) ~init:ns
    in
    let nodes = (G.nodes g) in
      List.fold_left nodes ~f:add_node_edges ~init:(NS.zero_node_score_map nodes)
end



(*****************************************************************)
(* KARMA                                                         *)
(* Random Walk Ranker                                            *)
(*****************************************************************)

module type WALK_PARAMS =
sig
  (* Should we randomly jump somewhere else occasionally?
    if no, this should be None.  Else it should be the probability of
    jumping on each step *)
  val do_random_jumps : float option
  (* How far must sisyphus walk? *)
  val num_steps : int
end


module RandomWalkRanker (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N)
  (P : WALK_PARAMS) :
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA

  let rank (g: G.graph) : NS.node_score_map =
    let get_next_node (v: G.node) : G.node = 
    match G.neighbors g v with
    | None -> failwith "Node not in graph"
    | Some [] ->
      (match G.get_random_node g with
       | None -> failwith "Graph is empty"
       | Some next_node -> next_node)
    | Some l -> 
      match (List.nth l (Random.int (List.length l))) with
      | None -> failwith "Error sampling neighbors"
      | Some next_node -> next_node
    in
    let rec gen_node_scores (n: int) (cur : G.node) (nsm : NS.node_score_map) :
      NS.node_score_map =
      if n = 0 then nsm
      else 
        let new_nsm = NS.add_score nsm cur 1. in
        let next = 
          match P.do_random_jumps with
          | Some alpha ->
            if Random.float 1. < alpha then 
              match G.get_random_node g with
              | None -> failwith "Graph is empty"
              | Some next_node -> next_node
            else get_next_node cur
          | None -> get_next_node cur
        in
        gen_node_scores (n-1) next new_nsm
    in
    match G.get_random_node g with
    | None -> NS.empty
    | Some init -> NS.normalize (gen_node_scores P.num_steps init
                          (NS.zero_node_score_map (G.nodes g)))

end


(*****************************************************************)
(* KARMA                                                         *)
(* Quantum Ranker                                                *)
(*****************************************************************)


module type QUANTUM_PARAMS =
sig
  (* What fraction of each node's score should be uniformly distributed
     to the graph as a whole? *)
  val alpha : float
  (* How many rounds? *)
  val num_steps : int

  (* Print stuff? *)
  val debug : bool
end


module QuantumRanker (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N)
  (P : QUANTUM_PARAMS) :
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA

  let propagate_weight (cur: G.node) (g: G.graph) (nsm: NS.node_score_map) 
    (prev: NS.node_score_map) : NS.node_score_map =
    let recipients = 
      match G.neighbors g cur with
      | None -> failwith "Node not in graph"
      | Some l -> 
        if List.exists ~f:(fun x -> x = cur) l then l
        else cur :: l
    in 
    let cur_score = 
      match NS.get_score prev cur with
      | None -> failwith "Node not in NodeScore"
      | Some x -> x
    in
    let out_score = cur_score *. (1. -. P.alpha) /.
                      (float (List.length recipients))
    in
    List.fold_left ~f:(fun nsm' v -> 
                         NS.add_score nsm' v out_score) ~init:nsm recipients

  let rank (g: G.graph) : NS.node_score_map =
    let nodes = G.nodes g in
    let num_nodes = List.length nodes in
    let base = P.alpha /. (float num_nodes) in
    let rec gen_node_scores (n: int) (nsm : NS.node_score_map) :
      NS.node_score_map =
      if n = 0 then nsm
      else 
        gen_node_scores (n-1)
          (NS.normalize
             (List.fold_left 
                ~f:(fun nsm' v ->
                      propagate_weight v g (NS.add_score nsm' v base) nsm)
                ~init:(NS.zero_node_score_map nodes) nodes))
    in
    gen_node_scores P.num_steps (NS.fixed_node_score_map (G.nodes g)
      (1. /. (float num_nodes)))

end



(*******************  TESTS BELOW  *******************)

module TestInDegreeRanker =
struct
  module G = NamedGraph
  let g = G.add_edge G.empty "a" "b";;
  let g2 = G.add_edge g "a" "c";;

  module NS = NodeScore (struct
                           type node = string
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = InDegreeRanker (G) (NS);;
  let ns = Ranker.rank g2;;
  let _ = Printf.printf "NS: %s\n" (NS.string_of_node_score_map ns) ;;
  assert ((NS.get_score ns "a") = Some 0.0);;
  assert ((NS.get_score ns "b") = Some 1.0);;
  assert ((NS.get_score ns "c") = Some 1.0);;
  assert ((NS.get_score ns "d") = None);;

  let g3 = G.add_edge g2 "b" "c";;
  let ns2 = Ranker.rank g3;;
  assert ((NS.get_score ns2 "a") = Some 0.0);;
  assert ((NS.get_score ns2 "b") = Some 1.0);;
  assert ((NS.get_score ns2 "c") = Some 2.0);;

end

module TestRandomWalkRanker =
struct
  module G = NamedGraph
  let g = G.from_edges [("a","b") ;
                        ("b","c") ;
                        ("c","d") ;
                        ("d","e") ;
                        ("e","f") ;
                        ("a","g") ;
                        ("g","a")]

  module NS = NodeScore (struct
                           type node = string
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = RandomWalkRanker (G) (NS)
    (struct
       let do_random_jumps = None
       let num_steps = 1000
     end)

  let ns = Ranker.rank g
  let _ = Printf.printf "Testing RandomWalkRanker:\n NS: %s\n"
    (NS.string_of_node_score_map ns)

(* That's the problem with randomness -- hard to test *)
end


module TestQuantumRanker =
struct
  module G = NamedGraph
  let g = G.from_edges [("a","b") ;
                        ("a","c") ;
                        ("b","c") ;
                        ("c","a")]

  module NS = NodeScore (struct
                           type node = string
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = QuantumRanker (G) (NS)
    (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)

  let ns = Ranker.rank g
  let _ = Printf.printf "Testing Quantum ranker:\n %s\n"
    (NS.string_of_node_score_map ns)

(* That's the problem with randomness -- hard to test *)
end
