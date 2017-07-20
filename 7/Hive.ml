open WorldObject
open WorldObjectI
open Event

(* ### Part 3 Actions ### *)
let starting_pollen = 500
let cost_of_bee = 10
let spawn_probability = 20
let pollen_probability = 50
let max_pollen_deposit = 3

(** A hive will spawn bees and serve as a deposit point for the pollen that bees
    harvest.  It is possible to steal honey from a hive, however the hive will
    signal that it is in danger and its loyal bees will become angry. *)
class hive p :
object
  inherit world_object_i

  method forfeit_honey : int -> world_object_i -> int
  method get_pollen_event : int Event.event
  method get_pollen : int
end =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable pollen = starting_pollen

  (* ### TODO: Part 6 Custom Events ### *)
  val pollen_event = Event.new_event ()

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event (fun _ -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private do_action : unit = 
    (if World.rand pollen_probability < 1 then
       pollen <- pollen + 1);
    (if World.rand spawn_probability < 1 && cost_of_bee < pollen then
       (self#generate_bee;
       pollen <- pollen - cost_of_bee))
    

  (* ### TODO: Part 4 Aging ### *)
 
  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)
  
  method private generate_bee =
    if World.rand 2 = 1 then
      ignore( new BeeBouncy.bee_bouncy self#get_pos (self :> world_object_i))
    else 
      ignore( new BeeRandom.bee_random self#get_pos (self :> world_object_i))

  (* ### TODO: Part 5 Smart Bees ### *)

  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "hive"

  method draw = self#draw_circle Graphics.cyan Graphics.black
                  (string_of_int pollen)

  method draw_z_axis = 1

  (* ### TODO: Part 3 Actions ### *)
  method receive_pollen ps =
   Event.fire_event self#get_pollen_event self#get_pollen;
   (if List.length ps > max_pollen_deposit then 
        pollen <- pollen + max_pollen_deposit
     else
       pollen <- pollen + (List.length ps));
   []

(*
   (if List.length ps > max_pollen_deposit then 
       (let rec remove_length lst n =
          if n = 0 then lst
          else 
            match lst with
            | hd :: tl -> remove_length tl (n-1)
            | _ -> lst
        in
        pollen <- pollen + max_pollen_deposit;
        remove_length ps max_pollen_deposit)
     else
       (pollen <- pollen + (List.length ps); []))
*)

  (* ### TODO: Part 6 Custom Events ### *)

  (************************)
  (***** Hive Methods *****)
  (************************)

  (* ### TODO: Part 3 Actions ### *)
  method forfeit_honey n thief =
    if pollen < n then 
      (let temp = pollen in
       pollen <- 0;
       self#danger thief;
       temp)
    else
      (pollen <- pollen - n;
       self#danger thief;
       n)


  (* ### TODO: Part 6 Custom Events ### *)
  method get_pollen_event = pollen_event

  method get_pollen = pollen


end
