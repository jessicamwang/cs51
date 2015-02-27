open Core.Std
open Util
open CrawlerServices
open Order
open Pagerank


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
   = InDegreeRanker (PageGraph) (PageScore) 
     (* = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end) *)
  (*
   = QuantumRanker (PageGraph) (PageScore) (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt _ () = gen_key ()
    let gen_key_lt _ () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between _ _ () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* Builds an index by doing the following:
 *
 * Remove a link from the frontier, visit this link, add outgoing links
 * to the frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  if n = 0 then d
  else 
    match LinkSet.choose frontier with
    | None -> d
    | Some (url, rem) -> 
      match CrawlerServices.get_page url with
      | None -> crawl n rem visited d
      | Some page ->
        let visited' = LinkSet.insert url visited in
        let frontier' = List.fold_left ~f:(fun f l -> 
                                             if LinkSet.member visited' l then
                                               f
                                             else LinkSet.insert l f)
                                       ~init:rem page.links in
        let d' = List.fold_left
                   ~f:(fun d_all w ->
                         match WordDict.lookup d_all w with
                         | None -> WordDict.insert d_all w
                                     (LinkSet.singleton url)
                         | Some v -> WordDict.insert d_all w
                                       (LinkSet.insert url v))
                   ~init:d page.words in
        crawl (n-1) frontier' visited' d'
;;

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
