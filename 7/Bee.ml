open WorldObject
open WorldObjectI
open Movable

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
class bee p : movable_t =
object (self)
  inherit movable p bee_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable pollen = []


  (* ### TODO: Part 5 Smart Bees ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event (fun _ -> self#do_action);


  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private do_action = 
    let neighbors = World.get self#get_pos in
    List.iter self#deposit_pollen neighbors;
    List.iter self#extract_pollen neighbors

  method private deposit_pollen neighbor : unit = 
    let rem = neighbor#receive_pollen pollen in
    pollen <- rem

  method private extract_pollen neighbor : unit =
    match neighbor#forfeit_pollen with
    | None -> ()
    | Some p -> pollen <- p :: pollen


  (* ### TODO: Part 5 Smart Bees ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "bee"

  (* ### TODO: Part 4 Aging ### *)
  method draw = self#draw_circle Graphics.yellow Graphics.black
                  (string_of_int (List.length pollen))

  method draw_z_axis = 2

  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method next_direction = Some (Direction.random Random.int)


  (* ### TODO: Part 5 Smart Bees ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees ### *)

end
