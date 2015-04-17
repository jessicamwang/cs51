open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let spawn_bear_pollen = 500

(** A cave will spawn a bear when the hive has collected a certain amount of
    honey. *)
class cave p hive : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 6 Custom Events ### *)
  initializer
    self#register_handler hive#get_pollen_event (fun _ -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)
  method private do_action : unit =
    if hive#get_pollen > spawn_bear_pollen && 
       not (World.fold (fun x b -> b || x#get_name = "bear") false)
    then
      (ignore(new Bear.bear self#get_pos hive (self :> world_object_i));
       print_string "omg bears! ";
       flush_all ())

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "cave"

  method draw = self#draw_circle Graphics.black Graphics.white "C"

  method draw_z_axis = 1


  (* ### TODO: Part 6 Custom Events *)

  method receive_pollen _ = []

end
