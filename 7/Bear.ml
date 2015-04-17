open WorldObject
open WorldObjectI
open Movable

(* ### Part 3 Actions ### *)
let pollen_theft_amount = 1000

(* ### Part 4 Aging ### *)
let bear_starting_life = 20

(* ### Part 2 Movement ### *)
let bear_inverse_speed = Some 10

class bear p hive home : movable_t =
object (self)
  inherit movable p bear_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable honey = 0

  (* ### TODO: Part 6 Events ### *)
  val mutable life = bear_starting_life
  
  val mutable alive = true;

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
    if self#get_pos = hive#get_pos then
       honey <- (honey + (hive#forfeit_honey pollen_theft_amount
                           (self :> world_object_i)))
    else if alive && self#get_pos = home#get_pos && 
            hive#get_pollen < pollen_theft_amount / 2 
            then
      (ignore(home#receive_pollen []);
       alive <- false;
       self#die)


  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "bear"

  method draw = self#draw_circle (Graphics.rgb 170 130 110) Graphics.black
                  (string_of_int honey)

  method draw_z_axis = 3


  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)
  method receive_sting =
    life <- life - 1;
    if life <= 0 && alive then
      (alive <- false;
       self#die)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method next_direction = 
    if honey = 0 then
      World.direction_from_to (self#get_pos) (hive#get_pos)
    else
      World.direction_from_to (self#get_pos) (home#get_pos)

  (* ### TODO: Part 6 Custom Events ### *)

end
