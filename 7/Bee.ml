open WorldObject
open WorldObjectI
open Movable
open Ageable
open CarbonBased

(* ### Part 2 Movement ### *)
let bee_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
let max_pollen_types = 5

(* ### Part 4 Aging ### *)
let bee_lifetime = 1000

(* ### Part 5 Smart Bees ### *)
let max_sensing_range = 5

(** Bees travel the world searching for honey.  They are able to sense flowers
    within close range, and they will return to the hive once they have
    pollenated enough species of flowers. *)
class type bee_t =
object
  inherit Ageable.ageable_t

  method private next_direction_default : Direction.direction option
end
class bee p (home:world_object_i) : bee_t =
object (self)
  
  inherit carbon_based p bee_inverse_speed (World.rand bee_lifetime) 
          bee_lifetime as super
  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable pollen = []


  (* ### TODO: Part 5 Smart Bees ### *)
  val sensing_range = World.rand max_sensing_range
  val pollen_types = World.rand max_pollen_types + 1

  (* ### TODO: Part 6 Custom Events ### *)
  val mutable target = None

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event (fun _ -> self#do_action);
    self#register_handler home#get_danger_event (fun x -> self#do_danger x);


  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)
  method private do_danger tar = 
    self#register_handler tar#get_die_event (fun x -> target <- None);
    target <- Some tar

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private do_action = 
    let neighbors = World.get self#get_pos in
    List.iter self#deposit_pollen neighbors;
    List.iter self#extract_pollen neighbors;
    match target with 
    | None -> ()
    | Some t -> 
      if self#get_pos = t#get_pos then 
        (t#receive_sting;
         self#die)

  method private deposit_pollen neighbor : unit = 
    let rem = neighbor#receive_pollen pollen in
    pollen <- rem

  method private extract_pollen neighbor : unit =
    match neighbor#forfeit_pollen with
    | None -> ()
    | Some p -> pollen <- p :: pollen


  (* ### TODO: Part 5 Smart Bees ### *)
  method private magnet_flower : world_object_i option =
    let objs = World.objects_within_range self#get_pos sensing_range in
    let fl = List.filter (fun x -> x#get_name = "flower" &&
                                   match x#smells_like_pollen with
                                   | None -> false
                                   | Some p -> not (List.mem p pollen))
                         objs
    in
    match fl with
    | [] -> None
    | hd :: _ -> Some (List.fold_left 
                         (fun f1 f2 -> 
                            if Direction.distance f1#get_pos self#get_pos < 
                               Direction.distance f2#get_pos self#get_pos
                            then f1
                            else f2)
                          hd fl)


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "bee"

  (* ### TODO: Part 4 Aging ### *)
  method draw_picture = self#draw_circle Graphics.yellow Graphics.black
                  (string_of_int (List.length pollen))

  method draw_z_axis = 2
  

  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method next_direction = 
    match target with 
    | Some t -> World.direction_from_to self#get_pos t#get_pos
    | None ->
      let rec check_unique_pollens p seen n =
        n = pollen_types || 
        match p with
        | [] -> false
        | hd :: tl -> if not (List.mem hd seen) then
                        check_unique_pollens tl (hd :: seen) (n+1)
                      else check_unique_pollens tl seen n
      in
      if check_unique_pollens pollen [] 0 then 
        World.direction_from_to self#get_pos home#get_pos
      else 
        match self#magnet_flower with
        | None -> self#next_direction_default
        | Some f -> World.direction_from_to self#get_pos f#get_pos


  (* ### TODO: Part 5 Smart Bees ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees ### *)
  method private next_direction_default = None

end
