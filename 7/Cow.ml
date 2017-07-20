open WorldObject
open WorldObjectI
open Movable

(* ### Part 2 Movement ### *)
let cow_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_consumed_objects = 100

(** Cows will graze across the field until it has consumed a satisfactory number
    of flowers *)
class cow p hive home : movable_t =
object (self)
  inherit movable p cow_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable eaten = 0

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
    if self#get_pos = home#get_pos && eaten = max_consumed_objects then
      self#die
    else
      let neighbors = World.get self#get_pos in
      let eat obj = 
        obj#die;
        eaten <- eaten + 1;
        print_string "*nom* ";
        flush_all ()
      in
      List.iter (fun x -> 
                   if eaten < max_consumed_objects then
                     match x#smells_like_pollen with
                     | None -> ()
                     | Some _ -> eat x) neighbors


  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "cow"

  method draw = self#draw_circle (Graphics.rgb 180 140 255) Graphics.black 
                  (string_of_int eaten)

  method draw_z_axis = 4


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)
  method next_direction = 
      if eaten = max_consumed_objects then
        World.direction_from_to (self#get_pos) (home#get_pos)
      else if World.rand World.size < 2 then
        World.direction_from_to (self#get_pos) (hive#get_pos)
      else Some (Direction.random Random.int)


  (* ### TODO: Part 6 Custom Events ### *)

end
